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

(define-condition auth::invalid-password (error)
  ())

(defclass user (user:user)
  ((entry :initarg :entry :accessor entry)))

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
  (setf *ldap* (ldap:new-ldap :host (config :ldap :host)
                              :port (config :ldap :port)
                              :sslflag (config :ldap :ssl)
                              :base (config :ldap :base)
                              :user (config :ldap :user)
                              :pass (config :ldap :pass)))
  (ldap:bind *ldap*))

(define-trigger server-stop ()
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
  (let ((user (user::ensure user)))
    (ldap:modify (entry user) *ldap*
                 `((ldap:replace :userpassword ,(hash-password password))))
    user))

(defun auth::check-password (user password)
  (let ((user (user::ensure user)))
    (unless (password-valid-p (first (ldap:attr-value (entry user) :userpassword))
                              password)
      (error 'auth::invalid-password))))

(defun user::default-perms ()
  (config :default-perms))

(defun (setf user::default-perms) (value)
  (setf (config :default-perms)
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
    (ldap:dosearch (entry (ldap:search *ldap* '(= objectclass radianceaccount)
                                       :size-limit 0 :paging-size 1000))
      (push (make-instance 'user :entry entry) users))
    users))

(defun user::create (username &key (if-exists :error))
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
                                          (:accountname . ,username)
                                          ,@(when (user::default-perms)
                                              `((:accountpermission ,@(user::default-perms)))))
                                 :infer-rdn NIL)))
      (ldap:add entry *ldap*)
      (make-instance 'user :entry entry))))

(defun user:get (username &key (if-does-not-exist NIL))
  (if (ldap:search *ldap* `(and (= objectclass "radianceAccount")
                                (= accountname ,username))
                   :size-limit 1)
      (make-instance 'user :entry (ldap:next-search-result *ldap*))
      (ecase if-does-not-exist
        (:create (user::create username))
        (:error (error 'user:not-found :name username))
        (:anonymous (user:get "anonymous"))
        ((NIL :NIL)))))

(defun user:remove (user)
  (let ((user (user::ensure user)))
    (ldap:delete (entry user) *ldap*)
    NIL))

(defun user:username (user)
  (let ((user (user::ensure user)))
    (first (ldap:attr-value (entry user) :accountname))))

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
    (mapcar #'decode-field (ldap:attr-value (entry user) :accountfield))))

(defun user:field (field user)
  (let ((user (user::ensure user))
        (enc (encode-field field)))
    (dolist (value (ldap:attr-value (entry user) :accountfield))
      (when (string= enc value :end2 (length enc))
        (return (nth-value 1 (decode-field value)))))))

(defun (setf user:field) (value field user)
  (let* ((user (user::ensure user))
         (enc (encode-field field))
         (prev (dolist (value (ldap:attr-value (entry user) :accountfield))
                 (when (string= enc value :end2 (length enc))
                   (return value)))))
    (ldap:modify (entry user) *ldap*
                 (if prev
                     `((ldap:delete :accountfield ,prev)
                       (ldap:add :accountfield ,(encode-field field value)))
                     `((ldap:add :accountfield ,(encode-field field value)))))
    value))

(defun user:remove-field (field user)
  (let ((user (user::ensure user)))
    (ldap:modify (entry user) *ldap* `((ldap:delete :accountfield ,(user:field field user))))
    user))

(defun encode-branch (branch)
  (etypecase branch
    (string (format NIL "~a." branch))
    (list (format NIL "~{~a.~}" branch))))

(defun user:check (user branch)
  (let* ((user (user::ensure user))
         (branch (encode-branch branch))
         (branches (ldap:attr-value (entry user) :accountpermission)))
    (dolist (b branches)
      (when (and (<= (length b) (length branch))
                 (string= b branch :end2 (length b)))
        (return user)))))

(defun user:grant (user &rest branches)
  (let ((user (user::ensure user)))
    (ldap:modify (entry user) *ldap*
                 (loop for branch in branches
                       collect `(ldap:add :accountpermission ,(encode-branch branch))))
    user))

(defun user:revoke (user &rest branches)
  (let* ((user (user::ensure user))
         (mods ()))
    (dolist (g (ldap:attr-value (entry user) :accountpermission))
      (dolist (r (mapcar #'encode-branch branches))
        (when (and (<= (length r) (length g))
                   (string= r g :end2 (length r)))
          (push `(ldap:delete :accountpermission ,g) mods))))
    (ldap:modify (entry user) *ldap* mods)
    user))

(defun user:add-default-permissions (&rest branches)
  (setf (user::default-perms) (append branches (user::default-perms))))
