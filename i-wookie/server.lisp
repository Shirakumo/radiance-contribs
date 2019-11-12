#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-wookie
  (:use #:cl #:radiance)
  (:implements #:server))
(in-package #:i-wookie)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf wookie:*enabled-plugins* '(:get :post :cookie :multipart))
  (wookie:load-plugins))

(defvar *listeners* (make-hash-table :test 'eql))
(defvar *listener-lock* (bt:make-lock "LISTENERS"))

(define-trigger server-start ()
  (defaulted-config `(:address "0.0.0.0" :port ,(or (mconfig :radiance-core :port) 8080)) :default)
  (defaulted-config `(:default) :enabled)
  (loop for name in (config :enabled)
        do (apply #'server:start name (config name))))

(define-trigger server-stop ()
  (mapcar #'server:stop (server:listeners)))

(defun server:start (name &key port address ssl-cert ssl-key ssl-pass)
  (check-type name keyword)
  (l:info :server "Starting listener ~a" name)
  (bt:make-thread
   #'(lambda ()
       (as:with-event-loop (:catch-app-errors T)
         (let* ((listener (if (and ssl-cert ssl-key)
                              (make-instance 'wookie:ssl-listener :port port :bind address :password ssl-pass :key ssl-key :certificate ssl-cert)
                              (make-instance 'wookie:listener :port port :bind address)))
                (server (wookie:start-server listener)))
           (bt:with-lock-held (*listener-lock*)
             (setf (gethash name *listeners*) server))))))
  (trigger 'server:started name))

(defun server:stop (name)
  (check-type name keyword)
  (let ((listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (bt:with-lock-held (*listener-lock*)
         (as:close-tcp-server listener)
         (remhash name *listeners*))
       (trigger 'server:stopped name))
      (T
       (error "No such listener found.")))))

(defun server:listeners ()
  (bt:with-lock-held (*listener-lock*)
    (loop for name being the hash-keys of *listeners*
          collect name)))

(defun merge-hash-tables (target source &optional (transform #'identity))
  (when (and source target)
    (maphash #'(lambda (k v) (setf (gethash k target) (funcall transform v))) source))
  target)

(defun set-real-cookie (cookie wk-response)
  (wookie-plugin-export:set-cookie
   wk-response
   (name cookie)
   (value cookie)
   :expires (expires cookie)
   :path (path cookie)
   :domain (domain cookie)
   :secure (secure cookie)
   :http-only (http-only cookie)))

(defun handle-request (wk-request wk-response)
  (let ((response (request (parse-uri (format NIL "~a~a"
                                              (gethash "host" (headers request))
                                              (urlencode:urldecode
                                               (subseq (wookie:request-resource wk-request) 0
                                                       (position #\? (wookie:request-resource wk-request)))
                                               :lenientp T)))
                           :http-method (wookie:request-method wk-request)
                           :headers (wookie:request-headers wk-request)
                           :post (cond ((search "application/x-www-form-urlencoded" (gethash "content-type" (headers request)))
                                        (wookie:plugin-request-data :post wk-request))
                                       ((search "multipart/form-data" (gethash "content-type" (headers request)))
                                        (merge-hash-tables (getf (wookie:plugin-request-data :multipart wk-request) :hash-file)
                                                           (getf (wookie:plugin-request-data :multipart wk-request) :hash-form))))
                           :get (wookie:plugin-request-data :get wk-request)
                           :cookies (wookie:plugin-request-data :cookie wk-request)
                           :remote "unknown")))
    (handler-bind ((error #'handle-condition))
      ;; Process attributes
      (maphash #'(lambda (key val) (declare (ignore key)) (set-real-cookie val wk-response)) (cookies response))
      (maphash #'(lambda (key val)
                   (push val (wookie:response-headers wk-response))
                   (push key (wookie:response-headers wk-response))) (headers response))
      ;; Process body
      (etypecase (data response)
        (pathname
         (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
               (output-stream (wookie:start-response wk-response :status (return-code response))))
           (with-open-file (input-stream (data response) :element-type '(unsigned-byte 8))
             (loop for n = (read-sequence buffer input-stream)
                   while (< 0 n) do
                     (write-sequence (subseq buffer 0 n) output-stream)
                     (force-output output-stream)))
           (wookie:finish-response wk-response)))
        
        ((or string (array (unsigned-byte 8)))
         (wookie:send-response wk-response :status (return-code response) :body (data response)))

        (null (error 'request-empty :request NIL)))
      wk-response)))

(wookie:defroute (:* ".*") (req res)
  (handler-bind ((error #'(lambda (err)
                            (cond
                              (*debugger* (invoke-debugger err))
                              (t (l:severe :server "Error at lowest level: ~a" err)
                                 (invoke-restart 'error-out))))))
    (restart-case 
        (handle-request req res)
      (error-out ()
        :report "Just error out ungracefully."
        (wookie:send-response res :status 500 :body "Something went very, very wrong.")))))
