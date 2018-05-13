#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-ldap
  (:use #:cl #:radiance)
  (:implements #:auth #:user)
  (:domain "auth"))
(in-package #:i-ldap)

(defvar *ldap*)
(defvar *lock* (bt:make-lock "LDAP lock"))
(defvar *locked* NIL)

;; FIXME: This is obviously inefficient since it
;;        bottlenecks everything to a single thread
;;        but doing it multi-threaded causes bad shit
(defmacro with-ldap (() &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk ()
              ,@body))
       (if *locked*
           (,thunk)
           (bt:with-lock-held (*lock*)
             (let ((*locked* T))
               (,thunk)))))))

(define-condition auth::invalid-password (api-error)
  ()
  (:default-initargs :message "Invalid username or password."))

(defclass user (user:user ldap:entry)
  ())

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a" (user:username user))))

(define-trigger server-start ()
  (defaulted-config "localhost" :ldap :host)
  (defaulted-config ldap::+ldap-port-no-ssl+ :ldap :port)
  (defaulted-config NIL :ldap :ssl)
  (defaulted-config NIL :ldap :base)
  (defaulted-config NIL :ldap :user)
  (defaulted-config NIL :ldap :pass)
  (defaulted-config "inetOrgPerson" :account :object-class)
  (defaulted-config :closed :account :registration)
  (defaulted-config "Radiance Account Recovery" :account :recovery :subject)
  (defaulted-config "Hi, ~a.

An account recovery was recently requested. If this was you, please
use the following link to recover our account. If you did not request
a recovery, you can simply ignore this email.

    ~a

Note that the recovery link will expire after 24 hours and you will
not be sent a new mail before then."
                    :account :recovery :message)
  (setf *ldap* (ldap:new-ldap :host (config :ldap :host)
                              :port (config :ldap :port)
                              :sslflag (config :ldap :ssl)
                              :base (config :ldap :base)
                              :user (config :ldap :user)
                              :pass (config :ldap :pass)))
  (ldap:bind *ldap*)
  (with-ldap ()
    (unless (ldap:search *ldap* '(= "objectClass" "radianceNextID") :size-limit 1)
      (ldap:add (ldap:new-entry (format NIL "cn=radianceNextID~@[,~a~]" (config :ldap :base))
                                :attrs '(("objectClass" . "radianceNextID")
                                         ("cn" . "radianceNextID")
                                         ("accountID" . "0"))
                                :infer-rdn NIL)
                *ldap*))
    (user::create "anonymous" :if-exists NIL))
  ;; Set this after the anonymous user creation to ensure it does not
  ;; get the password change permission.
  (defaulted-config (list "auth.change-password.") :account :default-perms)
  (trigger 'user:ready))

(define-trigger server-stop ()
  (trigger 'user:unready)
  (ldap:unbind *ldap*))

(defun auth:current (&optional default (session (session:get)))
  (or (session:field session 'user)
      (and default (user:get default :if-does-not-exist :error))))

(defun auth:associate (user &optional (session (session:get)))
  (l:info :auth "Associating ~a with ~a and prolonging for ~a"
          session user auth:*login-timeout*)
  (setf (session:field session 'user) user)
  (incf (session:timeout session)
        (case auth:*login-timeout*
          ((NIL) 0)
          ((T) (* 60 60 24 365 100))
          (otherwise auth:*login-timeout*)))
  (trigger 'auth:associate session))

(defun cat-vec (&rest vecs)
  (let ((arr (make-array (loop for v in vecs sum (length v))
                         :element-type (array-element-type (first vecs)))))
    (loop for i = 0 then (+ i (length v))
          for v in vecs
          do (replace arr v :start1 i))
    arr))

(defun hash-password (password &optional (salt (radiance:make-random-string 12)))
  (let ((salt (etypecase salt
                (null NIL)
                (string (babel:string-to-octets salt))
                ((simple-array (unsigned-byte 8)) salt)))
        (pass (babel:string-to-octets password)))
    (format NIL "~:[{SHA}~;{SSHA}~]~a" salt
            (base64:usb8-array-to-base64-string
             (cat-vec (ironclad:digest-sequence :sha1 (if salt (cat-vec pass salt) pass)) salt)))))

(defun password-valid-p (hash password)
  (cond ((string= "{SSHA}" hash :end2 6)
         (let ((code (base64:base64-string-to-usb8-array (subseq hash 6))))
           (string= hash (hash-password password (subseq code 20)))))
        ((string= "{SHA}" hash :end2 5)
         (string= hash (hash-password password NIL)))
        (T
         (error "Invalid hash."))))

(defun auth::set-password (user password)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:modify user *ldap*
                   `((ldap:replace :userpassword ,(hash-password password))))
      user)))

(defun auth::check-password (user password)
  (let ((user (user::ensure user)))
    (unless (password-valid-p (first (ldap:attr-value user :userpassword))
                              password)
      (error 'auth::invalid-password))))

(defun auth::recovery-active-p (user)
  (let* ((user (user::ensure user))
         (recovery (ldap:attr-value user :accountrecovery)))
    (not (null recovery))))

(defun auth::create-recovery (user)
  (with-ldap ()
    (let ((user (user::ensure user))
          (recovery (make-random-string :length 64)))
      (ldap:modify user *ldap* `((ldap:add :accountrecovery ,recovery)))
      recovery)))

(defun auth::recover (user code)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (recovery (find code (ldap:attr-value user :accountrecovery) :test #'string=))
           (new (make-random-string)))
      (unless recovery
        (error 'api-error :message "Invalid username or code."))
      (ldap:modify user *ldap* `((ldap:delete :accountrecovery ,recovery)))
      (auth::set-password user new)
      new)))

(defun get-next-id ()
  (with-ldap ()
    (loop (unless (ldap:search *ldap* '(= "objectClass" "radianceNextID")
                               :size-limit 1 :attributes '("accountID"))
            (error "LDAP is missing the required radianceNextID object."))
          (let* ((entry (ldap:next-search-result *ldap*))
                 (id (parse-integer (first (ldap:attr-value entry :accountid)))))
            (when (ldap:modify *ldap* entry `((ldap:delete "accountID" ,(princ-to-string id))
                                              (ldap:add "accountID" ,(princ-to-string (1+ id)))))
              (return id))))))

(defun user::default-perms ()
  (config :account :default-perms))

(defun (setf user::default-perms) (value)
  (setf (config :account :default-perms)
        (mapcar #'encode-branch value)))

(defun user::ensure (thing)
  (etypecase thing
    (user:user thing)
    (string (user:get thing :if-does-not-exist :error))))

(defun user:= (a b)
  (string= (user:username a)
           (user:username b)))

(defun user:list ()
  (let ((users ()))
    (with-ldap ()
      (ldap:dosearch (entry (ldap:search *ldap* '(= objectclass radianceaccount)
                                         :size-limit 0 :paging-size 1000))
        (push (change-class entry 'user) users)))
    users))

(defun user::create (username &key (if-exists :error))
  (with-ldap ()
    (let ((user (user:get username)))
      (when user
        (ecase if-exists
          (:supersede (user::remove user))
          (:error (error 'user::already-exists :name username))
          (:ignore (return-from user::create user))
          ((NIL :NIL) (return-from user::create NIL))))
      (let ((entry (ldap:new-entry (format NIL "cn=~a~@[,~a~]" username (config :ldap :base))
                                   :attrs `((:objectclass ,(config :account :object-class)
                                                          "radianceAccount")
                                            (:cn . ,username)
                                            (:sn . ,username)
                                            (:accountid . ,(princ-to-string (get-next-id)))
                                            (:accountname . ,username)
                                            ,@(when (user::default-perms)
                                                `((:accountpermission ,@(user::default-perms)))))
                                   :infer-rdn NIL)))
        (ldap:add entry *ldap*)
        (change-class entry 'user)))))

(defun user:get (username &key (if-does-not-exist NIL))
  (with-ldap ()
    (if (ldap:search *ldap* `(and (= objectclass "radianceAccount")
                                  (= accountname ,username))
                     :size-limit 1)
        (change-class (ldap:next-search-result *ldap*) 'user)
        (ecase if-does-not-exist
          (:create (user::create username))
          (:error (error 'user:not-found :name username))
          (:anonymous (user:get "anonymous"))
          ((NIL :NIL))))))

(defun user:remove (user)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:delete user *ldap*)
      NIL)))

(defun user:username (user)
  (let ((user (user::ensure user)))
    (first (ldap:attr-value user :accountname))))

(defun user::id (user)
  (let ((user (user::ensure user)))
    (first (ldap:attr-value user :accountid))))

(defun encode-field (field &optional value)
  (with-output-to-string (out)
    (loop for char across field
          do (when (char= char #\=)
               (write-char #\\ out))
             (write-char char out))
    (write-char #\= out)
    (when value (write-string value out))))

(defun decode-field (value)
  (with-input-from-string (in value)
    (values (with-output-to-string (out)
              (loop for char = (read-char in)
                    do (case char
                         (#\\ (write-char (read-char in) out))
                         (#\= (return))
                         (T (write-char char out)))))
            (with-output-to-string (out)
              (loop for char = (read-char in NIL)
                    while char do (write-char char out))))))

(defun user:fields (user)
  (let ((user (user::ensure user)))
    (mapcar #'decode-field (ldap:attr-value user :accountfield))))

(defun user:field (field user)
  (let ((user (user::ensure user))
        (enc (encode-field field)))
    (dolist (value (ldap:attr-value user :accountfield))
      (when (string= enc value :end2 (length enc))
        (return (nth-value 1 (decode-field value)))))))

(defun (setf user:field) (value field user)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (enc (encode-field field))
           (prev (dolist (value (ldap:attr-value user :accountfield))
                   (when (string= enc value :end2 (length enc))
                     (return value)))))
      (ldap:modify user *ldap*
                   (if prev
                       `((ldap:delete :accountfield ,prev)
                         (ldap:add :accountfield ,(encode-field field value)))
                       `((ldap:add :accountfield ,(encode-field field value)))))
      value)))

(defun user:remove-field (field user)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:modify user *ldap* `((ldap:delete :accountfield ,(user:field field user))))
      user)))

(defun encode-branch (branch)
  (etypecase branch
    (string (format NIL "~a." branch))
    (list (format NIL "~{~a.~}" branch))))

(defun user:check (user branch)
  (let* ((user (user::ensure user))
         (branch (encode-branch branch))
         (branches (ldap:attr-value user :accountpermission)))
    (dolist (b branches)
      (when (and (<= (length b) (length branch))
                 (string= b branch :end2 (length b)))
        (return user)))))

(defun user:grant (user &rest branches)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:modify user *ldap*
                   (loop for branch in branches
                         collect `(ldap:add :accountpermission ,(encode-branch branch))))
      user)))

(defun user:revoke (user &rest branches)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (mods ()))
      (dolist (g (ldap:attr-value user :accountpermission))
        (dolist (r (mapcar #'encode-branch branches))
          (when (and (<= (length r) (length g))
                     (string= r g :end2 (length r)))
            (push `(ldap:delete :accountpermission ,g) mods))))
      (ldap:modify user *ldap* mods)
      user)))

(defun user:add-default-permissions (&rest branches)
  (setf (user::default-perms) (append branches (user::default-perms))))
