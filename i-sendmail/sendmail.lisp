#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-sendmail
  (:use #:cl #:radiance)
  (:implements #:mail))
(in-package #:i-sendmail)

(define-trigger server-start ()
  (defaulted-config "test@radiance" :from)
  (defaulted-config NIL :reply-to)
  (defaulted-config "Radiance" :display-name)
  (defaulted-config :utf-8 :external-format))

(defun mail:send (to subject message)
  (trigger 'mail:send to subject message)
  (cl-sendmail:with-email (stream to
                           :subject subject
                           :from (format NIL "~@[~s ~]~a"
                                         (config :display-name) (config :from))
                           :reply-to (config :reply-to)
                           :charset (config :external-format))
    (write-string message stream)))
