(in-package #:modularize-user)
(define-module simple-users
  (:use #:cl #:radiance)
  (:implements #:user))
(in-package #:simple-users)

(defvar *user-cache* (make-hash-table :test 'equalp))
(defvar *default-permissions* ())

(defclass user (user:user)
  ((username :initarg :username :initform (error "USERNAME required.") :accessor username)
   (id :initarg :id :initform (error "ID required.") :accessor id)
   (fields :initarg :fields :initform (make-hash-table :test 'equalp) :accessor fields)
   (perms :initarg :perms :initform () :accessor perms)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream)
    (format stream "USER ~a" (username user))))

(ubiquitous:define-ubiquitous-writer user:user (object)
  (list (user:id object)))

(ubiquitous:define-ubiquitous-reader user:user (form)
  (user:get (first form) :if-does-not-exist :anonymous))

(defmethod initialize-instance :after ((user user) &key username id)
  (setf (gethash username *user-cache*) user)
  (setf (gethash id *user-cache*) user)
  ;; FIXME: Not sure if this is actually correct behaviour. Probably not.
  (unless (string= "anonymous" username)
    (apply #'user:grant user *default-permissions*)))

(defun ensure-user (thing)
  (etypecase thing
    (user:user thing)
    ((or string integer) (user:get thing :if-does-not-exist :error))))

(defun user:= (a b)
  (eql (ensure-user a)
       (ensure-user b)))

(defun user:list ()
  (loop for user being the hash-values of *user-cache* using (hash-key key)
	when (numberp key) collect user))

(defun user:get (username/id &key (if-does-not-exist NIL))
  (let ((username (etypecase username/id
                    (string (string-downcase username/id))
                    (integer username/id))))
    (or (gethash username *user-cache*)
        (ecase if-does-not-exist
          (:create (etypecase username
                     (string (user::create username))
                     (integer (error "Cannot create an inexistent user from a user ID."))))
          (:error (error 'user:not-found :name username))
          (:anonymous (user:get "anonymous"))
          ((NIL :NIL))))))

(defun user::create (username)
  (l:info :users "Creating new user ~s" username)
  (let ((user (make-instance 'user
                             :username username
                             :id (db:insert 'users `((username . ,username) (permissions . ""))))))
    (trigger 'user:create user)
    user))

(defun user:username (user)
  (etypecase user
    (integer (user:username (user:get user)))
    (string user)
    (user:user (username user))))

(defun user:id (user)
  (etypecase user
    (integer user)
    (string (user:id (user:get user)))
    (user:user
     (let ((id (id user)))
       (etypecase id
         (integer id)
         (string
          ;; KLUDGE: We assume a DB would not use anything but Alphanumerics for the ID.
          (parse-integer id :radix 36)))))))

(defun user:fields (user)
  (let ((fields (fields (ensure-user user))))
    (loop for field being the hash-keys of fields
          for value being the hash-values of fields
          collect (cons field value))))

(defun user:field (field user)
  (gethash (string field) (fields (ensure-user user))))

(defun (setf user:field) (value field user)
  (let ((user (ensure-user user)))
    (db:with-transaction ()
      (if (nth-value 1 (gethash field (fields user)))
          (db:update 'fields (db:query (:and (:= 'uid (id user)) (:= 'field field))) `((value . ,value)))
          (db:insert 'fields `((uid . ,(id user)) (field . ,field) (value . ,value))))
      (setf (gethash field (fields user)) value))))

(defun user:remove-field (field user)
  (let ((user (ensure-user user)))
    (db:with-transaction ()
      (db:remove 'fields (db:query (:and (:= 'uid (id user)) (:= 'field field))))
      (remhash (string field) (fields user)))
    user))

(defun user:remove (user)
  (let ((user (ensure-user user)))
    (db:with-transaction ()
      (db:remove 'fields (db:query (:= 'uid (id user))))
      (db:remove 'users (db:query (:= '_id (id user))))
      (remhash (id user) *user-cache*)
      (remhash (username user) *user-cache*)
      (trigger 'user:remove user)
      (setf (fields user) NIL
            (id user) NIL
            (perms user) NIL))
    user))

(defun save-perms (user)
  (db:update 'users (db:query (:= '_id (id user)))
             `((permissions . ,(format NIL "~{~{~a~^.~}~^~%~}" (perms user)))))
  user)

(defun ensure-branch (branch)
  (etypecase branch
    (string (cl-ppcre:split "\\." branch))
    (list (mapcar #'string-downcase branch))))

(defun branch-matches (permission branch)
  (when (<= (length permission) (length branch))
    (loop for leaf-a in permission
          for leaf-b in branch
          always (string-equal leaf-a leaf-b))))

(defun branch-equal (a b)
  (loop for i in a for j in b
        always (string-equal (string i) (string j))))

(defun user:check (user branch)
  (let ((user (ensure-user user))
        (branch (ensure-branch branch)))
    (or (not branch)
        (loop for perm in (perms user)
                thereis (branch-matches perm branch)))))

(defun user:grant (user &rest branches)
  (let ((user (ensure-user user)))
    (dolist (branch branches)
      (let ((branch (ensure-branch branch)))
        (l:debug :users "Granting ~s to ~a." branch user)
        (pushnew branch (perms user) :test #'branch-equal)))
    (save-perms user)))

(defun user:revoke (user &rest branches)
  (let ((user (ensure-user user)))
    (dolist (branch branches)
      (let ((branch (ensure-branch branch)))
        (l:debug :users "Revoking ~s from ~a." branch user)
        (setf (perms user)
              (remove-if (lambda (perm) (branch-matches perm branch)) (perms user)))))
    (save-perms user)))

(defun user:add-default-permissions (&rest branches)
  (dolist (branch branches)
    (pushnew branch *default-permissions* :test #'branch-equal))
  (loop with anonymous = (user:get "anonymous")
        for user being the hash-values of *user-cache*
        unless (eq user anonymous)
        do (apply #'user:grant user branches)))

(defun user::sync-user (username)
  (dm:with-model model ('users (db:query (:= 'username username)))
    (let ((user (make-instance 'user
                               :id (dm:id model) :username (dm:field model "username")
                               :perms (mapcar #'(lambda (b) (cl-ppcre:split "\\." b))
                                              (cl-ppcre:split "\\n" (dm:field model "permissions"))))))
      (dolist (entry (dm:get 'fields (db:query (:= 'uid (dm:id model)))))
        (let ((field (dm:field entry "field"))
              (value (dm:field entry "value")))
          (l:debug :users "Set field ~a of ~a to ~s" field user value)
          (setf (gethash field (fields user)) value)))
      user)))

(defun user::sync ()
  (setf *user-cache* (make-hash-table :test 'equalp))
  (let ((idtable (make-hash-table :test 'eql)))
    (dolist (model (dm:get 'users (db:query :all)))
      (l:debug :users "Loading ~a" (dm:field model "username"))
      (setf (gethash (dm:id model) idtable)
            (make-instance 'user
                           :id (dm:id model) :username (dm:field model "username")
                           :perms (mapcar #'(lambda (b) (cl-ppcre:split "\\." b))
                                          (cl-ppcre:split "\\n" (dm:field model "permissions"))))))
    ;; sync fields
    (dolist (entry (dm:get 'fields (db:query :all)))
      (let ((field (dm:field entry "field"))
            (value (dm:field entry "value"))
            (uid (dm:field entry "uid")))
        (l:debug :users "Set field ~a of ~a to ~s" field (gethash uid idtable) value)
        (setf (gethash field (fields (gethash uid idtable))) value)))
    ;; ensure anonymous user
    (user:get "anonymous" :if-does-not-exist :create)
    (l:info :users "Synchronized ~d users from database." (hash-table-count idtable))))

(define-trigger db:connected ()
  (db:create 'users '((username (:varchar 32)) (permissions :text)) :indices '(username))
  (db:create 'fields '((uid :integer) (field (:varchar 64)) (value :text)) :indices '(uid))
  (user::sync)
  (trigger 'user:ready))

(define-trigger db:disconnected ()
  (trigger 'user:unready))
