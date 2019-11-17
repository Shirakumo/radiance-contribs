#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module i-hunchentoot
  (:use #:cl #:radiance)
  (:implements #:server))
(in-package #:i-hunchentoot)

(defvar *listeners* (make-hash-table :test 'eql))

(defclass prng-safe-taskmaster (hunchentoot:one-thread-per-connection-taskmaster)
  ())

(defmethod hunchentoot:start-thread ((taskmaster prng-safe-taskmaster) thunk &key name)
  (flet ((thunk ()
           (let ((ironclad:*prng* (ironclad:make-prng :os)))
             (funcall thunk))))
    (bt:make-thread #'thunk :name name)))

(defclass radiance-acceptor (hunchentoot:acceptor)
  ()
  (:default-initargs
   :access-log-destination NIL
   :message-log-destination NIL
   :taskmaster (make-instance 'prng-safe-taskmaster)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor radiance-acceptor) request)
  (multiple-value-bind (response request) (handle-hunchentoot-request request)
    (handle-radiance-response response request)))

(defclass radiance-ssl-acceptor (radiance-acceptor hunchentoot:ssl-acceptor)
  ())

(define-trigger server-start ()
  (defaulted-config `(:default) :enabled)
  (defaulted-config `(:address "0.0.0.0" :port ,(or (mconfig :radiance-core :port) 8080)) :default)
  (loop for name in (config :enabled)
        do (apply #'server:start name (config name))))

(define-trigger server-stop ()
  (mapcar #'server:stop (server:listeners)))

(defun make-listener (port address ssl-cert ssl-key ssl-pass)
  (if (and ssl-cert ssl-key)
      (make-instance 'radiance-ssl-acceptor
                     :port port :address address
                     :ssl-certificate-file ssl-cert
                     :ssl-privatekey-file ssl-key
                     :ssl-privatekey-password ssl-pass)
      (make-instance 'radiance-acceptor 
                     :port port :address address)))

(defun server:start (name &key port address ssl-cert ssl-key ssl-pass)
  (check-type name keyword)
  (let ((listener (make-listener port address ssl-cert ssl-key ssl-pass)))
    (l:info :server "Starting listener ~a" name)
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)
    (trigger 'server:started name)))

(defun server:stop (name)
  (check-type name keyword)
  (let ((listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (hunchentoot:stop listener)
       (remhash name *listeners*)
       (trigger 'server:stopped name))
      (T
       (error "No such listener found.")))))

(defun server:listeners ()
  (loop for name being the hash-keys of *listeners*
        collect name))

(defun set-real-cookie (cookie)
  (declare (cookie cookie))
  (declare (optimize (speed 3)))
  (hunchentoot:set-cookie
   (name cookie) :value (value cookie) :expires (expires cookie)
                 :path (path cookie) :domain (domain cookie)
                 :secure (secure cookie) :http-only (http-only cookie)))

(defun header-encode (string)
  (with-output-to-string (out)
    (loop for octet across (flexi-streams:string-to-octets (or string "") :external-format :utf-8)
          for char = (code-char octet)
          do (if (< (char-code char) 128)
                 (write-char char out)
                 (format out "%~2,'0x" (char-code char))))))

(defconstant +default-buffer-size+ 4096)

(defun read-unprocessed-body (stream content-length)
  (let ((buffer (make-array +default-buffer-size+ :element-type (stream-element-type stream))))
    (loop for read = (read-sequence buffer stream :end (min +default-buffer-size+ content-length))
       until (= 0 read)
       do (decf content-length read))))

(defun handle-radiance-response (response request)
  (declare (optimize (speed 3)))
  (let ((*request* request)
        (*response* response))
    (restart-case
        (handler-bind
            ((error #'handle-condition))
          (when (body-stream request)
            (read-unprocessed-body (body-stream request) (content-length request)))
          (l:trace :server "Post-process: ~a" response)
          ;; Process attributes
          (setf (hunchentoot:return-code*) (return-code response)
                (hunchentoot:content-type*) (content-type response))
          (maphash (lambda (key val) (declare (ignore key)) (set-real-cookie val))
                   (cookies response))
          (maphash (lambda (key val) (setf (hunchentoot:header-out (header-encode key))
                                           (header-encode val)))
                   (headers response))
          (unless (data response)
            (error 'request-empty :request request)))
      (set-data (data)
        (setf (data response) data)))
    ;; Process body
    (etypecase (data response)
      (pathname (hunchentoot:handle-static-file (data response) (content-type response)))
      (string (data response))
      ((array (unsigned-byte 8)) (data response))
      (null "Something really bad is going on (empty request body after error handling)"))))

(defun handle-hunchentoot-request (request)
  (declare (optimize (speed 3)))
  (let* ((host (the string (hunchentoot:host request)))
         (colon (position #\: host)))
    #+sbcl (setf (sb-thread:thread-name (bt:current-thread)) host)
    (request (make-uri
              :domains (nreverse (cl-ppcre:split "\\." (string-downcase host :end colon)
                                                 :end (or colon (length host))))
              :port (when colon (parse-integer host :start (1+ colon)))
              ;; Cut off starting slash.
              :path (subseq (the string (hunchentoot:script-name request)) 1))
             :http-method (hunchentoot:request-method request)
             :body-stream (unless (hunchentoot:post-parameters request)
                            (hunchentoot:raw-post-data :request request
                                                       :want-stream t))
             :headers (hunchentoot:headers-in request)
             :post (hunchentoot:post-parameters request)
             :get (hunchentoot:get-parameters request)
             :cookies (hunchentoot:cookies-in request)
             :remote (hunchentoot:remote-addr request))))
