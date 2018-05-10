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

(defclass user (user:user)
  ((entry :initarg :entry :accessor entry)))

(define-trigger server-start ()
  (defaulted-config "localhost" :ldap :host)
  (defaulted-config ldap::+ldap-port-no-ssl+ :ldap :port)
  (defaulted-config NIL :ldap :ssl)
  (defaulted-config NIL :ldap :base)
  (defaulted-config NIL :ldap :user)
  (defaulted-config NIL :ldap :pass)
  (defaulted-config "inetOrgPerson")
  (setf *ldap* (ldap:new-ldap :host (config :ldap :host)
                              :port (config :ldap :port)
                              :sslflag (config :ldap :ssl)
                              :base (config :ldap :base)
                              :user (config :ldap :user)
                              :pass (config :ldap :pass)))
  (ldap:bind *ldap*))

(defun auth:current (&optional default (session (session:get)))
  (or (session:field session 'user)
      (and default (user:get default :if-does-not-exist :error))))

(defun auth:associate (user &optional (session (session:get)))
  (v:info :auth "Associating ~a with ~a and prolonging for ~a"
          session user auth:*login-timeout*)
  (setf (session:field session 'user) user)
  (incf (session:timeout session)
        (case auth:*login-timeout*
          ((NIL) 0)
          ((T) (* 60 60 24 365 100))
          (otherwise auth:*login-timeout*)))
  (trigger 'auth:associate session))

(defun default-perms ()
  )

(defun (setf default-perms) (value)
  )

(defun user::ensure (thing)
  (etypecase thing
    (user:user thing)
    (string (user:get thing :if-does-not-exist :error))))

(defun user:= (a b)
  (equal (user:username a)
         (user:username b)))

(defun user:list ()
  (ldap:dosearch (entry '(= objectclass radianceaccount))
    (make-instance 'user :entry entry)))

(defun user::create (username &key (if-exists :error))
  (let ((user (user:get username)))
    (when user
      (ecase if-exists
        (:supersede (user::remove user))
        (:error (error 'user::already-exists :name username))
        (:ignore (return-from 'user::create user))
        ((NIL :NIL) (return-from 'user::create NIL))))
    (let ((entry (ldap:new-entry username
                                 :attrs `(("objectClass" . "radianceAccount")
                                          ("accountName" . ,username)
                                          ("cn" . ,username)
                                          ("sn" . ,username)
                                          ,(default-perms)))))
      (ldap:add *ldap* entry)
      (make-instance 'user entry))))

(defun user:get (username &key (if-does-not-exist NIL))
  (or (ldap:dosearch (user `(and (= objectclass radianceaccount)
                                 (= accountname ,username)))
        (return user))
      (ecase if-does-not-exist
        (:create (user::create username))
        (:error (error 'user:not-found :name username))
        (:anonymous (user:get "anonymous"))
        ((NIL :NIL)))))

(defun user:remove (user)
  (let ((user (user::ensure user)))
    (ldap:delete *ldap* (entry user))))

(defun user:username (user)
  (let ((user (user::ensure user)))
    (ldap:attr-value (entry user) "accountName")))

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
  (let* ((user (user::ensure user))
         (attr (ldap:attr-value (entry user) :accountfield)))
    (loop for value in attr
          collect (decode-field value))))

(defun user:field (field user)
  (let ((user (user::ensure user))
        (enc (encode-field field)))
    (dolist (value (ldap:attr-value (entry user) :accountfield))
      (when (string= enc value :end2 (length enc))
        (nth-value 1 (decode-value value))))))

(defun (setf user:field) (value field user)
  (let ((user (user::ensure user)))
    (ldap:modify *ldap* (entry user) `((ldap:delete "accountField" ,(user:field field user))
                                       (ldap:add "accountField" ,(encode-field field value))))))

(defun user:remove-field (field user)
  (let ((user (user::ensure user)))
    (ldap:modify *ldap* (entry user) `((ldap:delete "accountField" ,(user:field field user))))))

(defun encode-branch (branch)
  (etypecase branch
    (string branch)
    (list (format NIL "~{~a~^.~}" branch))))

(defun matching-branches (a b)
  )

(defun user:check (user branch)
  (let* ((user (user::ensure user))
         (branches (ldap:attr-value (entry user) :accountpermission)))
    (not (null (matching-branches branches (list branch))))))

(defun user:grant (user &rest branches)
  (let ((user (user::ensure user)))
    (ldap:modify *ldap* (entry user)
                 (loop for branch in branches
                       collect `(ldap:add "accountPermission" ,(encode-branch branch))))))

(defun user:revoke (user &rest branches)
  (let* ((user (user::ensure user))
         (branches (ldap:attr-value (entry user) :accountpermission)))
    (ldap:modify *ldap* (entry user)
                 (loop for branch in (matching-branches branches perms)
                       collect `(ldap:delete "accountPermission" ,(encode-branch branch))))))

(defun user:add-default-permissions (&rest branches)
  (setf (default-perms)
        (append branches
                (default-perms))))
