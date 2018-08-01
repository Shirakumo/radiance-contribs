#|
 This file is a part of Radiance
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:oauth
  (:use #:cl #:radiance)
  (:export
   #:server
   #:application
   #:session
   #:*server*
   #:prune-sessions
   #:start-prune-thread
   #:stop-prune-thread))
(in-package #:oauth)

(defvar *prune-thread* NIL)

(define-trigger radiance:startup ()
  (defaulted-config (list (perm oauth authorize)
                          (perm oauth application))
                    :permissions :default)
  (defaulted-config (* 60 60)
                    :lifetime :unauth)
  (defaulted-config (* 60 60 24 365 1)
                    :lifetime :auth)
  (apply #'user:add-default-permissions (config :permissions :default))
  (start-prune-thread))

(define-trigger radiance:shutdown ()
  (stop-prune-thread))

(define-trigger db:connected ()
  (db:create 'applications '((key (:varchar 36))
                             (secret (:varchar 36))
                             (name (:varchar 32))
                             (description :text)
                             (author :integer))
             :indices '(key author))
  (db:create 'sessions '((key (:varchar 36))
                         (token (:varchar 36))
                         (secret (:varchar 36))
                         (verifier (:varchar 36))
                         (callback :text)
                         (access (:varchar 16))
                         (user :integer)
                         (expiry (:integer 5)))
             :indices '(key token)))

(defclass server (north:server)
  ())

(defclass application (north:application)
  ((description :initarg :description :accessor description)
   (author :initarg :author :accessor author)))

(defclass session (north:session)
  ((user :initarg :user :accessor user)
   (expiry :initarg :expiry :accessor expiry))
  (:default-initargs
   :user (error "USER required.")
   :expiry (+ (get-universal-time)
              (config :lifetime :unauth))))

(defvar *server* (make-instance 'server))

(defmethod north:make-application ((server server) &key name description author)
  (let ((application (make-instance 'application :name (or* name (error "NAME required."))
                                                 :description (or description "")
                                                 :author (or author (auth:current)
                                                             (error "AUTHOR required.")))))
    (l:info :oauth.application "Creating ~s" application)
    (db:insert 'applications `(("key" . ,(north:key application))
                               ("secret" . ,(north:secret application))
                               ("name" . ,(north:name application))
                               ("description" . ,(description application))
                               ("author" . ,(user:id (author application)))))
    application))

(defmethod north:make-session ((server server) application callback &key access user expiry)
  (let ((session (make-instance 'session :key application
                                         :callback callback
                                         :access access
                                         :user NIL
                                         :expiry (or expiry
                                                     (+ (get-universal-time)
                                                        (config :lifetime :unauth))))))
    (l:debug :oauth.session "Creating ~s" session)
    (db:insert 'sessions `(("key" . ,(north:key session))
                           ("token" . ,(north:token session))
                           ("secret" . ,(north:token-secret session))
                           ("verifier" . ,(north:verifier session))
                           ("callback" . ,(north:callback session))
                           ("access" . ,(north:access session))
                           ("user" . ,(when user (user:id user)))
                           ("expiry" . ,(expiry session))))
    session))

(defmethod north:application ((server server) application-key)
  (let ((data (first (db:select 'applications (db:query (:= 'key application-key)) :amount 1))))
    (when data
      (make-instance 'application :key (gethash "key" data)
                                  :secret (gethash "secret" data)
                                  :name (gethash "name" data)
                                  :description (gethash "description" data)))))

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
    (l:debug :oauth.session "Rehashing ~s" session)
    (setf (north:token session) (north:make-nonce))
    (setf (north:token-secret session) (north:make-nonce))
    (db:update 'sessions (db:query (:= 'token old-token))
               `(("token" . ,(north:token session))
                 ("secret" . ,(north:token-secret session)))
               :amount 1)
    session))

(defmethod north:revoke-application ((server server) application-key)
  (db:with-transaction ()
    (l:info :oauth.application "Revoking ~s" (north:application server application-key))
    (db:remove 'sessions (db:query (:= 'key application-key)))
    (db:remove 'applications (db:query (:= 'key application-key)))))

;; FIXME
(defmethod north:record-nonce ((server server) timestamp nonce))
(defmethod north:find-nonce ((server server) timestamp nonce))

(defun prune-sessions ()
  (l:info :oauth.prune "Pruning expired sessions.")
  (db:remove 'sessions (db:query (:< 'expiry (get-universal-time)))))

(defun start-prune-thread ()
  (when (and *prune-thread* (bt:thread-alive-p *prune-thread*))
    (error "Prune thread is already active."))
  (setf *prune-thread* T)
  (setf *prune-thread* (lambda ()
                         (unwind-protect
                              (with-simple-restart (abort-prune "Abort the prune thread")
                                (loop while *prune-thread*
                                      for i from 0
                                      do (sleep 1)
                                         (when (<= (* 60 60) i)
                                           (prune-sessions)
                                           (setf i 0))))
                           (l:debug :radiance.oauth.prune "Exiting prune thread."))))
  (l:info :oauth.prune "Prune thread started."))

(defun stop-prune-thread ()
  (when (and *prune-thread* (bt:thread-alive-p *prune-thread*))
    (let ((thread *prune-thread*))
      (setf *prune-thread* NIL)
      (loop repeat 100
            while (bt:thread-alive-p thread)
            do (sleep 0.001)
            finally (when (bt:thread-alive-p thread)
                      (bt:interrupt-thread thread (lambda ()
                                                    (invoke-restart 'abort-prune)))))
      (l:info :radiance.oauth.prune "Prune thread stopped."))))

(define-trigger auth:no-associated-user (session)
  (when (header "Authorization")
    (let ((oauth-session (north:oauth/verify *server* (translate-request *request*))))
      (auth:associate (user:get (user oauth-session)) session)
      (session:end session))))

(defun ->alist (&rest tables)
  (let ((alist ()))
    (dolist (table tables alist)
      (maphash (lambda (k v) (push (cons k v) alist)) table))))

(defun translate-request (request)
  (make-instance 'north:request
                 :http-method (http-method request)
                 :url (uri-to-url (uri request) :representation :external)
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
       ("oauth_callback_confirmed" . ,(if confirmed "true" "false"))))))

(define-oauth-api oauth/authorize (request oauth_token &optional action)
  (r-clip:with-clip-processing ("authorize.ctml")
    (let* ((session (north:session *server* oauth_token))
           (application (when session (north:application *server* (north:key session)))))
      (cond ((not application)
             (r-clip:process
              T :action :invalid
                :name "oAuth"
                :description ""))
            ((not action)
             (cond ((not (auth:current))
                    (redirect (resource :auth :page "login" (uri-to-url #@"oauth/api/oauth/authorize"
                                                                        :query `(("oauth_token" . ,oauth_token))
                                                                        :representation :external))))
                   ((user:check (auth:current) (perm oauth authorize))
                    (r-clip:process
                     T :action :authorize
                       :name (north:name application)
                       :description (description application)
                       :token oauth_token))
                   (T
                    (error 'request-denied :message "You are not permitted to perform oauth logins."))))
            ((string= action "allow")
             (multiple-value-bind (token verifier url) (north:oauth/authorize *server* request)
               (db:update 'sessions (db:query (:= 'token token))
                          `(("user" . ,(user:id (auth:current)))
                            ("expiry" . ,(+ (get-universal-time)
                                            (config :lifetime :auth)))) :amount 1)
               (when url (redirect url))
               (r-clip:process
                T :action :granted
                  :name (north:name application)
                  :description (description application)
                  :verifier verifier)))
            ((string= action "deny")
             (r-clip:process
              T :action :denied
                :name (north:name application)
                :description (description application)))))))

(define-oauth-api oauth/access-token (request)
  (multiple-value-bind (token secret) (north:oauth/access-token *server* request)
    (setf (content-type *response*) "text/plain")
    (north:alist->oauth-response
     `(("oauth_token" . ,token)
       ("oauth_token_secret" . ,secret)))))

(define-oauth-api oauth/verify (request)
  (let ((session (north:oauth/verify *server* request)))
    (setf (content-type *response*) "text/plain")
    (north:alist->oauth-response
     `(("oauth_verified" . "true")
       ("oauth_user" . ,(user:username (user session)))))))

(define-api oauth/application (&optional name key) (:access (perm oauth application))
  (unless (or name key)
    (error 'api-error :message "Either NAME or KEY must be given."))
  (let ((application (cond (name (db:select 'applications (db:query (:= 'name name)) :amount 1))
                           (key  (db:select 'applications (db:query (:= 'key key)) :amount 1)))))
    (if application
        (api-output (first application))
        (api-output NIL :status 404 :message "No such application found."))))

(define-api oauth/application/list/owned () (:access (perm oauth application))
  (api-output (db:select 'applications (db:query (:= 'author (user:id (auth:current)))))))

(define-api oauth/application/list/authorized () (:access (perm oauth application))
  (api-output (dolist (key (db:select 'sessions (db:query (:= 'user (user:id (auth:current)))) :fields '(key)))
                (let ((table (make-hash-table :test 'equal))
                      (application (north:application *server* key)))
                  (setf (gethash "key" table) key)
                  (setf (gethash "name" table) (north:name application))
                  (setf (gethash "description" table) (description application))
                  table))))

(define-api oauth/application/register (name &optional description) (:access (perm oauth application))
  (let ((application (north:make-application *server* :name name
                                                      :description description)))
    (if (string= (post/get "browser") "true")
        (redirect (uri-to-url #@"admin/oauth/applications" :query '(("info" . "Application registered."))
                                                           :representation :external))
        (api-output (first (db:select 'applications (db:query (:= 'key (north:key application))) :amount 1))))))

(define-api oauth/application/delete (key) (:access (perm oauth authorize))
  (north:revoke-application *server* key)
  (if (string= (post/get "browser") "true")
      (redirect (uri-to-url #@"admin/oauth/applications" :query '(("info" . "Application deleted."))
                                                         :representation :external))
      (api-output NIL)))

(define-api oauth/application/revoke (key) (:access (perm oauth authorize))
  (db:remove 'sessions (db:query (:= 'key key)))
  (if (string= (post/get "browser") "true")
      (redirect (uri-to-url #@"admin/oauth/authorizations" :query '(("info" . "Application revoked."))
                                                           :representation :external))
      (api-output NIL)))
