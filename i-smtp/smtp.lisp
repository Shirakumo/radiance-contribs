(in-package #:modularize-user)
(define-module #:i-smtp
  (:use #:cl #:radiance)
  (:implements #:mail))
(in-package #:i-smtp)

(define-trigger server-start ()
  (defaulted-config "localhost" :host)
  (defaulted-config "test@radiance" :from)
  (defaulted-config "Radiance" :display-name)
  (defaulted-config :utf-8 :external-format))

(defun mail:send (to subject message &key html)
  (trigger 'mail:send to subject message)
  (cl-smtp:send-email
   (config :host)
   (config :from)
   to
   subject
   message
   :html-message html
   :port (config :port)
   :ssl (config :ssl)
   :reply-to (config :reply-to)
   :display-name (config :display-name)
   :authentication (when (config :auth)
                     (list (config :auth :method)
                           (config :auth :username)
                           (config :auth :password)))
   :external-format (config :external-format)))
