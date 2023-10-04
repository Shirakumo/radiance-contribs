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

(defun activation-code (timeout)
  (format NIL "~a~36r" (make-random-string 32) (+ (get-universal-time) timeout)))

(defun activation-code-valid-p (code &optional user-code)
  (and (< (get-universal-time) (parse-integer (subseq code 32) :radix 36))
       (or (null user-code) (string= code user-code))))

(defclass user (user:user ldap:entry)
  ())

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type T)
    (format stream "~a" (user:username user))))

(defun auth:current (&optional default session)
  (let ((user (or (session:field session 'user)
                  (progn (trigger 'auth:no-associated-user)
                         (session:field session 'user)))))
    (or (when user
          (let ((user (user::ensure user)))
            (cond ((user::account-active-p user)
                   user)
                  (T
                   (l:info :radiance.ldap "Deleting user ~a as the account has expired" user)
                   #++(user:remove user)
                   NIL))))
        (and default (user:get default :if-does-not-exist :error)))))

(defun auth:associate (user &optional (session (session:get)))
  (l:debug :auth "Associating ~a with ~a and prolonging for ~a"
           session user auth:*login-timeout*)
  (setf (session:field session 'user) (user:username user))
  (incf (session:timeout session)
        (case auth:*login-timeout*
          ((NIL) 0)
          ((T) (* 60 60 24 365 100))
          (otherwise auth:*login-timeout*)))
  (trigger 'auth:associate session))

(defun auth::set-password (user password)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (ldap:modify user *ldap*
                   `((ldap:replace :userpassword ,(cryptos:to-base64 (cryptos:rfc-2307-hash password)))))
      user)))

(defun auth::check-password (user password &optional (errorp T))
  (let* ((user (user::ensure user))
         (hash (first (ldap:attr-value user :userpassword))))
    (if (and hash (cryptos:check-rfc-2307-hash password (cryptos:from-base64 hash)))
        T
        (when errorp (error 'auth::invalid-password)))))

(defun auth::recovery-active-p (user &optional user-code)
  (activation-code-valid-p (first (ldap:attr-value (user::ensure user) :accountrecovery)) user-code))

(defun auth::create-recovery (user)
  (with-ldap ()
    (let ((user (user::ensure user))
          (recovery (activation-code (config :account :recovery :timeout))))
      (ldap:modify user *ldap* `((ldap:replace :accountrecovery ,recovery)))
      recovery)))

(defun auth::recover (user code &optional errorp)
  (with-ldap ()
    (let ((user (user::ensure user))
          (new (make-random-string)))
      (cond ((auth::recovery-active-p user code)
             (ldap:modify user *ldap* `((ldap:delete :accountrecovery ,code)))
             (auth::set-password user new)
             new)
            (errorp
             (error 'api-error :message "Invalid username or code."))))))

(defun auth::totp-uri (user &optional otp-key)
  (cryptos:totp-uri (user:username user)
                    :secret (cryptos:from-base32
                             (or otp-key (first (ldap:attr-value (user::ensure user) :accounttotpkey)))
                             :octets)
                    :issuer (config :account :totp :issuer)
                    :digest (config :account :totp :digest)
                    :period (config :account :totp :period)
                    :digits (config :account :totp :digits)))

(defun auth::totp (user &optional otp-key)
  (cryptos:totp (cryptos:from-base32
                 (or otp-key
                     (or (first (ldap:attr-value (user::ensure user) :accounttotpkey))
                         (error 'api-error :message "Invalid username or code.")))
                 :octets) 
                :digest (config :account :totp :digest)
                :period (config :account :totp :period)
                :digits (config :account :totp :digits)))

(defun auth::totp-active-p (user)
  (first (ldap:attr-value (user::ensure user) :accounttotpkey)))

(defun (setf auth::totp-active-p) (value user)
  (if value
      (auth::activate-totp user)
      (with-ldap ()
        (let* ((user (user::ensure user))
               (key (first (ldap:attr-value user :accounttotpkey))))
          (when key
            (ldap:modify user *ldap* `((ldap:delete :accounttotpkey ,key)))
            NIL)))))

(defun auth::activate-totp (user &optional otp-key)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (or (first (ldap:attr-value user :accounttotpkey))
          (let ((key (or otp-key (cryptos:to-base32 (cryptos:make-salt 10)))))
            (ldap:modify user *ldap* `((ldap:replace :accounttotpkey ,key)))
            key)))))

(defun get-next-id ()
  (with-ldap ()
    (loop (unless (ldap:search *ldap* '(= "objectClass" "radianceNextID")
                               :size-limit 1 :attributes '("accountID") :base (config :ldap :base))
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
        (remove-duplicates
         (mapcar #'encode-branch value)
         :test #'string=)))

(defun user::ensure (thing)
  (etypecase thing
    (user:user thing)
    (T (user:get thing :if-does-not-exist :error :allow-inactive T))))

(defun user::account-active-p (user &optional user-code)
  (let* ((user (user::ensure user))
         (code (first (ldap:attr-value user :accountactivation))))
    (or (null code) (activation-code-valid-p code user-code))))

(defun (setf user::account-active-p) (value user)
  (with-ldap ()
    (let* ((user (user::ensure user))
           (code (first (ldap:attr-value user :accountactivation))))
      (if (or (null code) (activation-code-valid-p code))
          (unless value
            (ldap:modify user *ldap* `((ldap:replace :accountactivation ,(activation-code -1)))))
          (when value
            (ldap:modify user *ldap* `((ldap:delete :accountactivation ,code)))))
      value)))

(defun user::activate (user code &optional errorp)
  (with-ldap ()
    (let ((user (user::ensure user)))
      (cond ((user::account-active-p user code)
             (ldap:modify user *ldap* `((ldap:delete :accountactivation ,code)))
             user)
            (errorp
             (error 'api-error :message "Invalid username or code."))))))

(defun user:= (a b)
  (string= (user:username a)
           (user:username b)))

(defun user:list ()
  (let ((users ()))
    (with-ldap ()
      (ldap:dosearch (entry (ldap:search *ldap* '(= objectclass radianceaccount)
                                         :size-limit 0 :paging-size 1000 :base (config :ldap :base)))
        (push (change-class entry 'user) users)))
    users))

(defun user::create (username &key (if-exists :error) (email "") activate)
  (with-ldap ()
    (let ((user (user:get username))
          (code (activation-code (config :account :activation :timeout))))
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
                                            ,@(unless activate
                                                `((:accountactivation . ,code)))
                                            ,@(when (user::default-perms)
                                                `((:accountpermission ,@(user::default-perms)))))
                                   :infer-rdn NIL)))
        (ldap:add entry *ldap*)
        (let ((user (change-class entry 'user)))
          (setf (user:field "registration-date" user) (princ-to-string (get-universal-time)))
          (trigger 'user:create user)
          (when (null activate)
            (send-email user "email-account-registration.ctml"
                        :activation-url (uri-to-url "/api/auth/activate"
                                                    :representation :external
                                                    :query `(("browser" . "true")
                                                             ("username" . ,username)
                                                             ("code" . ,code)))))
          user)))))

(defun user:get (username/id &key (if-does-not-exist NIL) allow-inactive)
  (with-ldap ()
    (or (when (ldap:search *ldap* (etypecase username/id
                                    (string `(and (= objectclass "radianceAccount")
                                                  (= accountname ,username/id)))
                                    (integer `(and (= objectclass "radianceAccount")
                                                   (= accountid ,(princ-to-string username/id)))))
                           :size-limit 1 :base (config :ldap :base))
          (let ((user (change-class (ldap:next-search-result *ldap*) 'user)))
            (cond ((or (user::account-active-p user) allow-inactive)
                   user)
                  (T
                   (l:info :radiance.ldap "Deleting user ~a as the account has expired" user)
                   (user:remove user)
                   NIL))))
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
           (first (ldap:attr-value user :mail)))
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
    (string (format NIL "~a~@[.~]" branch (char/= #\. (char branch (1- (length branch))))))
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
  (defaulted-config (* 24 60 60) :account :activation :timeout)
  (defaulted-config :sha1 :account :totp :digest)
  (defaulted-config 6 :account :totp :digits)
  (defaulted-config 30 :account :totp :period)
  (defaulted-config "radiance" :account :totp :issuer)
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
    (unless (ldap:search *ldap* '(= "objectClass" "radianceNextID") :size-limit 1 :base (config :ldap :base))
      (ldap:add (ldap:new-entry (format NIL "cn=radianceNextID~@[,~a~]" (config :ldap :base))
                                :attrs '(("objectClass" . "radianceNextID")
                                         ("cn" . "radianceNextID")
                                         ("accountID" . "0"))
                                :infer-rdn NIL)
                *ldap*))
    (user::create "anonymous" :if-exists NIL))
  ;; Set this after the anonymous user creation to ensure it does not
  ;; get the password change permission.
  (defaulted-config (list "auth.change-password." "auth.totp.") :account :default-perms)
  (trigger 'user:ready))

(define-trigger server-stop ()
  (trigger 'user:unready)
  (loop for ldap = (pop *pool*)
        while ldap do (ldap:unbind ldap)))
