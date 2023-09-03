(in-package #:i-ldap)

(defvar *nonce-salt* (make-random-string))

(defun send-email (user template &rest args)
  (when (mail:implementation)
    (let* ((data (apply #'clip:process (plump:parse (@template template))
                        :user user
                        :username (user:username user)
                        args)))
      (mail:send (user:field "email" user)
                 (lquery:$1 data "title" (text))
                 (lquery:$1 data "body" (text))))))

(define-resource-locator auth page (page &optional landing)
  (cond ((find page '("login" "logout" "register" "recover") :test #'string-equal)
         (values (make-uri :domains '("auth") :path page)
                 `(("landing-page" . ,(etypecase landing
                                        (null "")
                                        (string (if (string= landing "#") "REFERER" landing))
                                        (uri (uri-to-url landing :representation :external)))))))
        (T (call-default-locator))))

(defun redirect-to-landing (&optional (other #@"/"))
  (let ((landing (or* (post/get "landing-page")
                      (session:field 'landing-page)
                      other)))
    (setf (session:field 'landing-page) NIL)
    (redirect landing)))

(defun maybe-save-landing ()
  (let ((landing (if (string= (post/get "landing-page") "REFERER")
                     (referer *request*)
                     (or* (post/get "landing-page")))))
    (when landing (setf (session:field 'landing-page) landing))))

(define-api auth/totp/login (totp) ()
  (when (auth:current)
    (error 'api-error :message "You are already logged in."))
  (let ((user (session:field 'totp-user)))
    (unless user
      (l:warn :radiance.ldap "(~a) TOTP login attempt failed: no user in session"
              (remote *request*) (user:username user))
      (error 'auth::invalid-password))
    (unless (= (parse-integer totp) (auth::totp user))
      (l:warn :radiance.ldap "(~a) TOTP login attempt for ~s failed: TOTP was ~6,'0d but expected ~6,'0d"
              (remote *request*) (user:username user) (parse-integer totp) (auth::totp user))
      (error 'auth::invalid-password))
    (l:info :radiance.ldap "(~a) TOTP Login attempt for user account ~s succeeded."
            (remote *request*) (user:username user))
    (auth:associate user)
    (if (string= "true" (post/get "browser"))
        (redirect-to-landing (if (admin:implementation)
                                 (resource :admin :page)
                                 #@"auth/login"))
        (api-output "Login successful."))))

(define-api auth/totp/qr (&optional totp-key) (:access (perm auth totp))
  (let* ((matrix (lispqr:encode->matrix (auth::totp-uri (auth:current) totp-key)))
         (png (make-instance 'zpng:png :color-type :grayscale
                                       :width (array-dimension matrix 1)
                                       :height (array-dimension matrix 0))))
    (dotimes (y (zpng:height png))
      (dotimes (x (zpng:width png))
        (setf (aref (zpng:image-data png) (+ x (* y (zpng:rowstride png))))
              (* 255 (- 1 (aref matrix y x))))))
    (lambda (stream) (zpng:write-png-stream png stream))))

(define-api auth/totp/update (action totp &optional totp-key) (:access (perm auth totp))
  (let ((user (auth:current)))
    (cond ((string-equal "enable" action)
           (unless (= (parse-integer totp) (auth::totp user totp-key))
             (l:warn :radiance.ldap "(~a) TOTP enable for ~s failed: TOTP was ~6,'0d but expected ~6,'0d"
                     (remote *request*) (user:username user) (parse-integer totp) (auth::totp user totp-key))
             (error 'auth::invalid-password))
           (l:info :radiance.ldap "(~a) Requesting TOTP enable for ~s"
                   (remote *request*) (user:username user))
           (auth::activate-totp (auth:current) totp-key)
           (send-email user "email-totp-activation.ctml"
                       :settings-url (multiple-value-bind (uri query fragment)
                                         (resource :admin :page "settings" "password")
                                       (uri-to-url uri :representation :external :query query :fragment fragment))))
          ((string-equal "disable" action)
           (unless (= (parse-integer totp) (auth::totp user))
             (l:warn :radiance.ldap "(~a) TOTP disable for ~s failed: TOTP was ~6,'0d but expected ~6,'0d"
                     (remote *request*) (user:username user) (parse-integer totp) (auth::totp user))
             (error 'auth::invalid-password))
           (l:info :radiance.ldap "(~a) Requesting TOTP disable for ~s"
                   (remote *request*) (user:username user))
           (setf (auth::totp-active-p (auth:current)) NIL)
           (send-email user "email-totp-deactivation.ctml"
                       :settings-url (multiple-value-bind (uri query fragment)
                                         (resource :admin :page "settings" "password")
                                       (uri-to-url uri :representation :external :query query :fragment fragment))))
          (T (error 'api-error :message "Invalid action ~s" action)))
    (if (string= "true" (post/get "browser"))
        (multiple-value-bind (uri query fragment)
            (resource :admin :page "settings" "password")
          (redirect (uri-to-url uri
                                :representation :external
                                :query `(("info" . ,(if (string-equal "enable" action)
                                                        "2-Factor authentication enabled"
                                                        "2-Factor authentication disabled"))
                                         ,@query)
                                :fragment fragment)))
        (api-output "Password changed."))))

(define-api auth/login (username password) ()
  (when (auth:current) (error 'api-error :message "You are already logged in."))
  (let ((user (user:get username)))
    (unless user
      (l:warn :radiance.ldap "(~a) Login attempt for user account ~s failed: no such account"
              (remote *request*) username)
      (error 'auth::invalid-password))
    (unless (auth::check-password user password NIL)
      (l:warn :radiance.ldap "(~a) Login attempt for user account ~s failed: invalid password"
              (remote *request*) username)
      (error 'auth::invalid-password))
    (session:start)
    (cond ((auth::totp-active-p user)
           (setf (session:field 'totp-user) user)
           (if (string= "true" (post/get "browser"))
               (redirect #@"auth/login/totp")
               (api-output "OTP")))
          (T
           (l:info :radiance.ldap "(~a) Login attempt for user account ~s succeeded."
                   (remote *request*) username)
           (auth:associate user)
           (if (string= "true" (post/get "browser"))
               (redirect-to-landing (if (admin:implementation)
                                        (resource :admin :page)
                                        #@"auth/login"))
               (api-output "Login successful."))))))

(define-api auth/logout () ()
  (session:end)
  (api-output "Logged out."))

(define-api auth/change-password (old-password new-password &optional repeat) (:access (perm auth change-password))
  (let ((user (auth:current NIL)))
    (unless user
      (error 'api-error :message "You are not logged in."))
    (unless (<= 8 (length new-password))
      (error 'api-error "Password must be 8 characters or more."))
    (unless (or (not repeat) (string= new-password repeat))
      (error 'api-error "The confirmation does not match the password."))
    (unless (auth::check-password user old-password NIL)
      (l:warn :radiance.ldap "(~a) Password change attempt for user account ~s failed: invalid old password"
              (remote *request*) (user:username user))
      (error 'auth::invalid-password))
    (l:info :radiance.ldap "(~a) Changed password for user account ~s"
            (remote *request*) (user:username user))
    (auth::set-password user new-password)
    (send-email user "email-password-change.ctml"
                :recovery-url (uri-to-url #@"auth/recover" :representation :external))
    (if (string= "true" (post/get "browser"))
        (multiple-value-bind (uri query fragment)
            (resource :admin :page "settings" "password")
          (redirect (uri-to-url uri
                                :representation :external
                                :query `(("info" . "Your password has been changed.")
                                         ,@query)
                                :fragment fragment)))
        (api-output "Password changed."))))

(define-api auth/request-recovery (username) ()
  (when (auth:current) (error 'api-error :message "You are already logged in."))
  (let ((user (user:get username)))
    (unless (and user (mail:implementation))
      (error 'api-error :message "Recovery is not possible at this time."))
    (when (auth::recovery-active-p user)
      (error 'api-error :message "A recovery email has already been sent."))
    (l:info :radiance.ldap "(~a) Requested recovery for user account ~s"
            (remote *request*) (user:username user))
    (let ((code (auth::create-recovery user)))
      (send-email user "email-account-recovery.ctml"
                  :recovery-url (uri-to-url "/api/auth/recover"
                                            :representation :external
                                            :query `(("browser" . "true")
                                                     ("username" . ,username)
                                                     ("code" . ,code))))
      (if (string= "true" (post/get "browser"))
          (redirect (uri-to-url #@"auth/recover"
                                :representation :external
                                :query `(("info" . "A recovery email has been sent."))))
          (api-output "Recovery email sent.")))))

(define-api auth/recover (username code) ()
  (when (auth:current)
    (error 'api-error :message "You are already logged in."))
  (let ((user (user:get username)))
    (unless user
      (l:warn :radiance.ldap "(~a) Recovery attempt for user account ~s failed: no such account"
              (remote *request*) (user:username user))
      (error 'api-error :message "Invalid username or code."))
    (let ((new-pw (auth::recover user code NIL)))
      (unless new-pw
        (l:warn :radiance.ldap "(~a) Recovery attempt for user account ~s failed: bad recovery code"
                (remote *request*) (user:username user))
        (error 'api-error :message "Invalid username or code."))
      (l:info :radiance.ldap "(~a) Recovering user account ~s"
              (remote *request*) (user:username user))
      (auth:associate user)
      (if (string= "true" (post/get "browser"))
          (redirect
           (if (admin:implementation)
               (multiple-value-bind (uri query fragment)
                   (resource :admin :page "settings" "password")
                 (uri-to-url uri
                             :representation :external
                             :query `(("password" . ,new-pw)
                                      ("info" . "Please set your new password now.")
                                      ,@query)
                             :fragment fragment))
               (uri-to-url "auth/recover"
                           :representation :external
                           :query `(("password" . ,new-pw)))))
          (api-output new-pw)))))

(define-api auth/activate (username code) ()
  (let ((user (user:get username)))
    (unless user
      (l:warn :radiance.ldap "(~a) Activation attempt for user account ~s failed: no such account"
              (remote *request*) (user:username user))
      (error 'api-error :message "Invalid username or code."))
    (unless (user::activate user code)
      (l:warn :radiance.ldap "(~a) Activation attempt for user account ~s failed: bad activation code"
              (remote *request*) (user:username user))
      (error 'api-error :message "Invalid username or code."))
    (l:info :radiance.ldap "(~a) Activating user account ~s"
            (remote *request*) (user:username user))
    (if (string= "true" (post/get "browser"))
        (redirect
         (if (admin:implementation)
             (multiple-value-bind (uri query fragment)
                 (resource :admin :page "settings" "password")
               (uri-to-url uri
                           :representation :external
                           :query `(("info" . "Your account has been activated.")
                                    ,@query)
                           :fragment fragment))
             (uri-to-url "/" :representation :external)))
        (api-output "Account activated"))))

(define-page login "auth/login" (:clip "login.ctml")
  (maybe-save-landing)
  (if (auth:current)
      (redirect-to-landing)
      (r-clip:process T)))

(define-page totp-login "auth/login/totp" (:clip "login-totp.ctml")
  (cond ((auth:current)
         (redirect-to-landing))
        ((null (session:field 'totp-user))
         (redirect #@"auth/login"))
        (T
         (r-clip:process T
                         :totp-placeholder (format NIL "~v{0~}" (config :account :totp :digits) 0)
                         :totp-digits (config :account :totp :digits)))))

(define-page logout "auth/logout" ()
  (session:end)
  (redirect-to-landing))

(define-page register "auth/register" (:clip "register.ctml")
  (maybe-save-landing)
  (ecase (config :account :registration)
    (:closed (r-clip:process T))
    (:open
     (with-actions (error info)
         ((:register
           (r-ratify:with-form
               (((:string 1 32) username)
                ((:email 1 64) email)
                ((:string 8 64) password repeat)
                (:nonce firstname))
             (declare (ignore firstname))
             (when (user:get username)
               (error "Sorry, the username is already taken."))
             (when (string/= password repeat)
               (error "The passwords do not match!"))
             (let ((user (user::create username :email email)))
               (auth::set-password user password)
               (auth:associate user)
               (redirect-to-landing (if (admin:implementation) (resource :admin :page) #@"/"))))))
       (let ((nonce (make-random-string)))
         (setf (session:field :nonce-hash) (cryptos:pbkdf2-hash nonce *nonce-salt*)
               (session:field :nonce-salt) *nonce-salt*)
         (r-clip:process
          T :msg (or error info)
            :user (auth:current)
            :nonce nonce))))))

(define-page recover "auth/recover" (:clip "recover.ctml")
  (maybe-save-landing)
  (r-clip:process
   T :password (post/get "password")
   :user (auth:current)))

(define-implement-trigger admin
  (admin:define-panel settings password (:access (perm auth change-password)
                                         :clip "settings.ctml"
                                         :icon "fa-key"
                                         :tooltip "Change your login password.")
    (r-clip:process
     T :info (post/get "info")
       :error (post/get "error")
       :totp-placeholder (format NIL "~v{0~}" (config :account :totp :digits) 0)
       :totp-digits (config :account :totp :digits)
       :totp-key (string-right-trim "=" (cryptos:to-base32 (cryptos:make-salt 10))))))
