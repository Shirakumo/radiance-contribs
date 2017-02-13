#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-auth
  (:use #:cl #:radiance)
  (:implements #:auth)
  (:domain "auth"))
(in-package #:simple-auth)

(define-trigger startup ()
  (defaulted-config (make-random-string) :salt)
  (defaulted-config "open" :registration))

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

(defun auth::set-password (user password)
  (setf (user:field "simple-auth-hash" user)
        (cryptos:pbkdf2-hash password (config :salt))))

(defun auth::check-password (user password)
  (and (user:field "simple-auth-hash" user)
       (string= (user:field "simple-auth-hash" user)
                (cryptos:pbkdf2-hash password (config :salt)))
       T))

(define-api simple-auth/login (username password &optional redirect) ()
  (flet ((err (message)
           (l:info :auth "Failed login for ~a." username)
           (cond
             (redirect
              (redirect (format NIL "~a?msg=~a" (referer *request*) message))
              (return-from simple-auth/login ""))
             (T (error 'api-error :message message)))))
    (when (auth:current)
      (err "Already logged in."))
    (let ((user (user:get username)))
      (unless user
        (err "Invalid username or password."))
      (let ((hash (user:field "simple-auth-hash" user)))
        (unless hash
          (err "Invalid username or password."))
        (cond
          ((string= hash (cryptos:pbkdf2-hash password (config :salt)))
           (auth:associate user)
           (let ((landing (or (session:field 'landing-page) "/")))
             (setf (session:field 'landing-page) NIL)
             (if redirect
                 (redirect landing)
                 (api-output "Login successful."))))
          (T
           (err "Invalid username or password.")))))))

(define-api simple-auth/logout () ()
  (if (auth:current)
      (progn (session:end)
             (api-output "Logged out."))
      (error 'api-error :message "You are not logged in.")))

(define-page login "auth/login" (:clip "login.ctml")
  (when (or* (post/get "landing-page"))
    (setf (session:field 'landing-page)
          (if (string= (post/get "landing-page") "REFERER")
              (referer *request*)
              (post/get "landing-page"))))
  (when (auth:current)
    (let ((landing (session:field 'landing-page)))
      (when landing
        (redirect landing))))
  (r-clip:process
   T
   :user (auth:current)
   :msg (get-var "msg")))

(define-resource-locator auth page (page &optional landing)
  (cond ((string-equal page "login")
         (values #@"auth/login"
                 `(("landing-page" . ,(etypecase landing
                                        (null "")
                                        (string
                                         (if (string= landing "#")
                                             (if (boundp '*request*)
                                                 (uri-to-url (uri *request*) :representation :external)
                                                 "REFERER")
                                             landing))
                                        (uri (uri-to-url landing :representation :external)))))))
        (T (call-default-locator))))

(define-page logout "auth/logout" ()
  (session:end)
  (redirect (or (session:field 'landing-page) "/")))

(defvar *nonce-salt* (make-random-string))
(define-page register "auth/register" (:clip "register.ctml")
  (if (string-equal (config :registration) "open")
      (with-actions (error info)
          ((:register
            (r-ratify:with-form
                (((:string 1 32) username)
                 ((:email 1 32) email)
                 ((:string 8 64) password repeat)
                 (:nonce firstname))
              (declare (ignore firstname))
              (when (user:get username)
                (error "Sorry, the username is already taken!"))
              (when (string/= password repeat)
                (error "The passwords do not match!"))
              (let ((user (user:get username :if-does-not-exist :create)))
                (setf (user:field "email" user) email)
                (auth::set-password user password)
                (auth:associate user)))))
        (let ((nonce (make-random-string)))
          (setf (session:field :nonce-hash) (cryptos:pbkdf2-hash nonce *nonce-salt*)
                (session:field :nonce-salt) *nonce-salt*)
          (r-clip:process
           T
           :msg (or error info)
           :user (auth:current)
           :nonce nonce)))
      (r-clip:process T)))

(define-trigger user:ready ()
  (user:add-default-permissions (perm auth change-password)))

(define-implement-trigger admin
  (admin:define-panel settings password (:access (perm auth change-password) :clip "settings.ctml" :icon "fa-key" :tooltip "Change your login password.")
    (let ((info) (error)
          (user (auth:current)))
      (handler-case
          (when (string= (post-var "action") "Save")
            (unless (<= 8 (length (or (post-var "new") "")))
              (error "Password must be 8 characters or more."))
            (unless (auth::check-password user (or (post-var "old") ""))
              (error "Old password is invalid."))
            (unless (string= (or (post-var "new") "") (or (post-var "repeat") ""))
              (error "New password fields do not match."))
            (auth::set-password user (post-var "repeat"))
            (setf info "Password updated!"))
        (error (err)
          (setf error (princ-to-string err))))
      (r-clip:process T :info info :error error))))
