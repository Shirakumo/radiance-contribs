#|
 This file is a part of Radiance
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:r-oauth
  (:use #:cl #:radiance)
  (:export
   #:server
   #:application
   #:session
   #:*server*))
(in-package #:r-oauth)

(define-trigger db:connected ()
  (db:create 'applications '((key (:varchar 36))
                             (secret (:varchar 36))
                             (name (:varchar 32)))
             :indices '(key name))
  (db:create 'sessions '((key (:varchar 36))
                         (token (:varchar 36))
                         (secret (:varchar 36))
                         (verifier (:varchar 36))
                         (callback :text)
                         (access (:varchar 16))
                         (user :integer))
             :indices '(key token)))

(defclass server (north:server)
  ())

(defclass application (north:application)
  ())

(defclass session (north:session)
  ((user :initarg :user :accessor user))
  (:default-initargs :user (error "USER required.")))

(defvar *server* (make-instance 'server))

(defmethod north:make-application ((server server) &key name)
  (let ((application (make-instance 'application :name name)))
    (db:insert 'applications `(("key" . ,(north:key application))
                               ("secret" . ,(north:secret application))
                               ("name" . ,(north:name application))))
    application))

(defmethod north:make-session ((server server) application callback &key access user)
  (let ((session (make-instance 'session :callback callback
                                         :access access
                                         :user user)))
    (db:insert 'applications `(("key" . ,(north:key session))
                               ("token" . ,(north:token session))
                               ("secret" . ,(north:token-secret session))
                               ("verifier" . ,(north:verifier session))
                               ("callback" . ,(north:callback session))
                               ("access" . ,(north:access session))
                               ("user" . ,(user:id user))))
    session))

(defmethod north:application ((server server) application-key)
  (let ((data (first (db:select 'applications (db:query (:= 'key application-key)) :amount 1))))
    (when data
      (make-instance 'application :key (gethash "key" data)
                                  :secret (gethash "secret" data)
                                  :name (gethash "name" data)))))

(defmethod north:session ((server server) token)
  (let ((data (first (db:select 'sessions (db:query (:= 'token token)) :amount 1))))
    (when data
      (make-instance 'session :token (gethash "token" data)
                              :token-secret (gethash "secret" data)
                              :verifier (gethash "verifier" data)
                              :callback (gethash "callback" data)
                              :key (gethash "key" data)
                              :access (gethash "access" data)
                              :user (gethash "user" data)))))

(defmethod north:rehash-session ((server server) (session session))
  (let ((old-token (north:token session)))
    (setf (north:token session) (north:make-nonce))
    (setf (north:token-secret session) (north:make-nonce))
    (db:update 'sessions (db:query (:= 'token old-token))
               `(("token" . ,(north:token session))
                 ("secret" . ,(north:token-secret session)))
               :amount 1)
    session))

(defmethod north:revoke-application ((server server) application-key)
  (db:with-transaction ()
    (db:remove 'sessions (db:query (:= 'key application-key)))
    (db:remove 'applications (db:query (:= 'key application-key)))))

;; FIXME
(defmethod north:record-nonce ((server server) timestamp nonce))
(defmethod north:find-nonce ((server server) timestamp nonce))

;; FIXME: Inject into auth/session.

(defun ->alist (&rest tables)
  (let ((alist ()))
    (dolist (table tables alist)
      (maphash (lambda (k v) (push (cons k v) aist)) table))))

(defun translate-request (request)
  (make-instance 'north:request
                 :http-method (http-method request)
                 :url "" ;; FIXME: Original URL?
                 :parameters (->alist (post-data request)
                                      (get-data request))
                 :headers (->alist (headers request))))

(defun call-with-oauth-handling (function)
  (handler-case (funcall function (translate-request *request*))
    (north:parameter-error (e)
      (setf (return-code *response*) 400)
      (radiance:api-output :status 400
                           :message (princ-to-string e)))
    (north:parameters-missing (e)
      (setf (return-code *response*) 400)
      (radiance:api-output :status 400
                           :message (princ-to-string e)
                           :data (north:parameters e)))
    (north:verification-error (e)
      (setf (return-code *response*) 401)
      (radiance:api-output :status 401
                           :message (princ-to-string e)))))

(defmacro define-oauth-api (endpoint (request &rest args) &body body)
  `(define-api ,endpoint ,args ()
     (call-with-oauth-handling
      (lambda (,request)
        ,@body))))

(define-oauth-api oauth/request-token (request)
  (multiple-value-bind (token secret confirmed) (north:oauth/request-token *server* request)
    (setf (content-type *response*) "text/plain")
    (north:alist->oauth-response
     `(("oauth_token" . ,token)
       ("oauth_token_secret" . ,secret)
       ("oauth_callback_confirmed" . ,confirmed)))))

(define-oauth-api oauth/authorize (request token &optional action)
  (cond ((not action)
         ;; RENDER PAGE
         )
        ((string= action "allow")
         (multiple-value-bind (token verifier url) (north:oauth/authorize *server* request)
           (if url
               (redirect url)
               ;; RENDER PAGE
               )))
        ((string= action "deny")
         ;; RENDER PAGE
         )))

(define-oauth-api oauth/access-token (request)
  (multiple-value-bind (token secret) (north:oauth/access-token *server* request)
    (setf (content-type *response*) "text/plain")
    (north:alist->oauth-response
     `(("oauth_token" . ,token)
       ("oauth_token_secret" . ,secret)))))

(define-oauth-api oauth/verify (request)
  (north:oauth/verify *server* request)
  (setf (content-type *response*) "text/plain")
  (north:alist->oauth-response
   `(("oauth_verified" . "true"))))
