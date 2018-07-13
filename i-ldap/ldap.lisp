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

(defvar *ldap* NIL)
(defvar *pool-lock* (bt:make-lock "LDAP pool lock"))
(defvar *pool-available-condition* (bt:make-condition-variable :name "LDAP pool condition"))
(defvar *pool* ())

(defun acquire-connection ()
  (bt:with-lock-held (*pool-lock*)
    (loop for con = (pop *pool*)
          until con
          do (l:debug :radiance.ldap.connection "~a Waiting..." (bt:current-thread))
             (bt:condition-wait *pool-available-condition* *pool-lock* :timeout 5)
          finally (return con))))

(defun release-connection (con)
  (bt:with-lock-held (*pool-lock*)
    (push con *pool*))
  (bt:condition-notify *pool-available-condition*))

(defun call-with-reconnection (function &optional (ldap *ldap*))
  (loop
     (handler-case
         (return-from call-with-reconnection
           (funcall function))
       (usocket:socket-error (err)
         (l:debug :radiance.ldap.connection err)
         (l:warn :radiance.ldap.connection "Error during LDAP operations: ~a" err)
         (loop (handler-case
                   (return (ldap::possibly-reopen-and-rebind ldap))
                 (usocket:socket-error (err)
                   (l:severe :radiance.ldap.connection "Failed to reconnect to LDAP: ~a" err)
                   (l:debug :radiance.ldap.connection err))))))))

(defun call-with-ldap (function &optional (ldap *ldap*))
  (if ldap
      (call-with-reconnection function ldap)
      (let ((*ldap* (acquire-connection)))
        (unwind-protect
             (call-with-reconnection function *ldap*)
          (release-connection *ldap*)))))

(defmacro with-ldap ((&optional (ldap '*ldap*)) &body body)
  `(call-with-ldap (lambda () ,@body) ,ldap))

(defmacro with-reconnection ((&optional (ldap '*ldap*)) &body body)
  `(call-with-reconnection (lambda () ,@body) ,ldap))

(define-condition auth::invalid-password (api-error)
  ()
  (:default-initargs :message "Invalid username or password."))

(defclass user (user:user ldap:entry)
  ())

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a" (user:username user))))

(defun auth:current (&optional default (session (session:get)))
  (let ((user (session:field session 'user)))
    (if user
        (user:get user)
        (and default (user:get default :if-does-not-exist :error)))))

(defun auth:associate (user &optional (session (session:get)))
  (l:info :auth "Associating ~a with ~a and prolonging for ~a"
          session user auth:*login-timeout*)
  (setf (session:field session 'user) (user:username user))
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

(defun encode-hash (digest salt hash)
  (format NIL "{~@[S~*~]~a}~a" salt
          (ecase digest
            (:md5 "MD5")
            (:sha1 "SHA")
            (:sha256 "SHA256")
            (:sha384 "SHA384")
            (:sha512 "SHA512"))
          (base64:usb8-array-to-base64-string
           (if salt (cat-vec hash salt) hash))))

(defun decode-hash (hash)
  (let ((closing (position #\} hash)))
    (if closing
        (let ((digest (subseq hash 1 closing))
              (hash (base64:base64-string-to-usb8-array
                     (subseq hash (1+ closing)))))
          (if (find digest '("SMD5" "SSHA" "SSHA256" "SSHA384" "SSHA512") :test #'string=)
              (let* ((digest (subseq digest 1))
                     (digest (if (string= "SHA" digest) :sha1 (find-symbol digest :KEYWORD))))
                (values (subseq hash 0 (ironclad:digest-length digest))
                        (subseq hash (ironclad:digest-length digest))
                        digest))
              (values hash
                      NIL
                      (if (string= "SHA" digest) :sha1 (find-symbol digest :KEYWORD)))))
        (values hash NIL NIL))))

(defun hash-password (password &key (salt (radiance:make-random-string 12))
                                    (digest :sha1))
  (let ((salt (etypecase salt
                (null NIL)
                (string (babel:string-to-octets salt))
                ((simple-array (unsigned-byte 8)) salt)))
        (pass (babel:string-to-octets password)))
    (encode-hash digest salt
                 (ironclad:digest-sequence digest (if salt (cat-vec pass salt) pass)))))

(defun password-valid-p (hash password)
  (multiple-value-bind (_ salt digest) (decode-hash hash)
    (declare (ignore _))
    (string= hash (hash-password password :salt salt :digest digest))))

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

(defun auth::recovery-active-p (user &optional code)
  (let* ((user (user::ensure user))
         (recovery (first (ldap:attr-value user :accountrecovery))))
    (and recovery
         (< (get-universal-time)
            (parse-integer (subseq recovery 32) :radix 36))
         (or (null code)
             (string= code recovery)))))

(defun auth::create-recovery (user)
  (with-ldap ()
    (let ((user (user::ensure user))
          (recovery (format NIL "~a~36r"
                            (make-random-string 32)
                            (+ (get-universal-time)
                               (config :account :recovery :timeout)))))
      (ldap:modify user *ldap* `((ldap:replace :accountrecovery ,recovery)))
      recovery)))

(defun auth::recover (user code)
  (with-ldap ()
    (let ((user (user::ensure user))
          (new (make-random-string)))
      (unless (auth::recovery-active-p user code)
        (error 'api-error :message "Invalid username or code."))
      (ldap:modify user *ldap* `((ldap:delete :accountrecovery ,code)))
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
    (T (user:get thing :if-does-not-exist :error))))

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

(defun user::create (username &key (if-exists :error) (email ""))
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
                                            (:mail . ,email)
                                            (:accountid . ,(princ-to-string (get-next-id)))
                                            (:accountname . ,username)
                                            ,@(when (user::default-perms)
                                                `((:accountpermission ,@(user::default-perms)))))
                                   :infer-rdn NIL)))
        (ldap:add entry *ldap*)
        (let ((user (change-class entry 'user)))
          (trigger 'user:create user)
          user)))))

(defun user:get (username/id &key (if-does-not-exist NIL))
  (with-ldap ()
    (if (ldap:search *ldap* (etypecase username/id
                              (string `(and (= objectclass "radianceAccount")
                                            (= accountname ,username/id)))
                              (integer `(and (= objectclass "radianceAccount")
                                             (= accountid ,(princ-to-string username/id)))))
                     :size-limit 1)
        (change-class (ldap:next-search-result *ldap*) 'user)
        (ecase if-does-not-exist
          (:create (etypecase username/id
                     (string (user::create username/id))
                     (integer (error "Cannot create an inexistent user from a user ID."))))
          (:error (error 'user:not-found :name username/id))
          (:anonymous (user:get "anonymous"))
          ((NIL :NIL))))))

(defun user:remove (user)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:delete user *ldap*)
      (trigger 'user:remove user)
      NIL)))

(defun user:id (user)
  (etypecase user
    (user:user (parse-integer (first (ldap:attr-value user :accountid))))
    (string    (user:id (user::ensure user)))
    (integer   user)))

(defun user:username (user)
  (etypecase user
    (user:user (first (ldap:attr-value user :accountname)))
    (string    user)
    (integer   (user:username (user::ensure user)))))

(defun encode-field (field &optional value)
  (with-output-to-string (out)
    (loop for char across (etypecase field
                            (symbol (string-downcase field))
                            (string field))
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
    (list* (first (ldap:attr-value user :mail))
           (mapcar #'decode-field (ldap:attr-value user :accountfield)))))

(defun prefix-p (prefix string)
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun user:field (field user)
  (let ((user (user::ensure user)))
    (cond ((string-equal field "email")
           (ldap:attr-value user :mail))
          (T
           (let ((enc (encode-field field)))
             (dolist (value (ldap:attr-value user :accountfield))
               (when (prefix-p enc value)
                 (return (nth-value 1 (decode-field value))))))))))

(defun (setf user:field) (value field user)
  (with-ldap ()
    (cond ((string-equal field "email")
           (let ((user (user::ensure user)))
             (ldap:modify user *ldap*
                          `((ldap:delete :mail ,(first (ldap:attr-value user :mail)))
                            (ldap:add :mail ,value)))))
          (T
           (let* ((user (user::ensure user))
                  (enc (encode-field field))
                  (prev (dolist (value (ldap:attr-value user :accountfield))
                          (when (prefix-p enc value)
                            (return value)))))
             (ldap:modify user *ldap*
                          (if prev
                              `((ldap:delete :accountfield ,prev)
                                (ldap:add :accountfield ,(encode-field field value)))
                              `((ldap:add :accountfield ,(encode-field field value))))))))
    value))

(defun user:remove-field (field user)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (enc (encode-field field))
           (prev (dolist (value (ldap:attr-value user :accountfield))
                   (when (prefix-p enc value)
                     (return value)))))
      (ldap:modify user *ldap* `((ldap:delete :accountfield ,prev)))
      user)))

(defun encode-branch (branch)
  (etypecase branch
    (string (format NIL "~a." branch))
    (list (with-output-to-string (out)
            (dolist (item branch)
              (write-string (string-downcase item) out)
              (write-char #\. out))))))

(defun user:check (user branch)
  (let* ((user (user::ensure user))
         (branch (encode-branch branch))
         (branches (ldap:attr-value user :accountpermission)))
    (dolist (b branches)
      (when (or (string= b ".")
                (and (<= (length b) (length branch))
                     (string= b branch :end2 (length b))))
        (return user)))))

(defun user:grant (user &rest branches)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (existing (ldap:attr-value user :accountpermission)))
      (ldap:modify user *ldap*
                   (loop for item in branches
                         for branch = (encode-branch item)
                         unless (find branch existing :test #'string=)
                         collect `(ldap:add :accountpermission ,branch)))
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


(define-trigger server-start ()
  (defaulted-config "localhost" :ldap :host)
  (defaulted-config ldap::+ldap-port-no-ssl+ :ldap :port)
  (defaulted-config NIL :ldap :ssl)
  (defaulted-config NIL :ldap :base)
  (defaulted-config NIL :ldap :user)
  (defaulted-config NIL :ldap :pass)
  (defaulted-config "inetOrgPerson" :account :object-class)
  (defaulted-config :closed :account :registration)
  (defaulted-config (* 24 60 60) :account :recovery :timeout)
  (defaulted-config "Radiance Account Recovery" :account :recovery :subject)
  (defaulted-config "Hi, ~a.

An account recovery was recently requested. If this was you, please
use the following link to recover your account. If you did not request
a recovery, you can simply ignore this email.

    ~a

Note that the recovery link will expire after 24 hours and you will
not be sent a new mail before then."
                    :account :recovery :message)
  (dotimes (i (defaulted-config 5 :ldap :connections))
    (let ((ldap (ldap:new-ldap :host (config :ldap :host)
                               :port (config :ldap :port)
                               :sslflag (config :ldap :ssl)
                               :base (config :ldap :base)
                               :user (config :ldap :user)
                               :pass (config :ldap :pass)
                               :timeout 5
                               :reuse-connection 'ldap:rebind)))
      (ldap:bind ldap)
      (push ldap *pool*)))
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
