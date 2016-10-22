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

(defvar *listeners* (make-hash-table :test 'equalp))

(defun mklist (port address ssl-cert ssl-key ssl-pass)
  (let ((args `(:port ,port :address ,address
                :access-log-destination NIL
                :message-log-destination NIL)))
    (if (and ssl-cert ssl-key)
        (apply #'make-instance 'hunchentoot:ssl-acceptor
               :ssl-certificate-file ssl-cert :ssl-privatekey-file ssl-key :ssl-privatekey-password ssl-pass args)
        (apply #'make-instance 'hunchentoot:easy-acceptor args))))

(defun server:start (port &key address ssl-cert ssl-key ssl-pass)
  (let ((listener (mklist port address ssl-cert ssl-key ssl-pass))
        (name (format NIL "~a:~a" address port)))
    (l:info :server "Starting listener ~a" name)
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)
    (trigger 'server:started port address)))

(defun server:stop (port &optional address)
  (let* ((name (format NIL "~a:~a" address port))
         (listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (hunchentoot:stop listener)
       (remhash name *listeners*)
       (trigger 'server:stopped port address))
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

(defun post-handler (response request)
  (declare (optimize (speed 3)))
  (handler-bind
      ((error #'handle-condition))
    (l:trace :server "Post-process: ~a" response)
    ;; Process attributes
    (setf (hunchentoot:return-code*) (return-code response)
          (hunchentoot:content-type*) (content-type response))
    (maphash #'(lambda (key val) (declare (ignore key)) (set-real-cookie val)) (cookies response))
    (maphash #'(lambda (key val) (setf (hunchentoot:header-out (header-encode key))
                                       (header-encode val))) (headers response))
    ;; Process body
    (etypecase (data response)
      (pathname (hunchentoot:handle-static-file (data response) (content-type response)))
      (string (data response))
      ((array (unsigned-byte 8)) (data response))
      (null (error 'request-empty :request request)))))

(defun pre-handler (request)
  (declare (optimize (speed 3)))
  (let* ((host (the string (hunchentoot:host request)))
         (colon (position #\: host)))
    #+sbcl (setf (sb-thread:thread-name (bt:current-thread)) host)
    #'(lambda () (post-handler (request (make-uri
                                         :domains (nreverse (cl-ppcre:split "\\." (string-downcase host :end colon)
                                                                            :end (or colon (length host))))
                                         :port (when colon (parse-integer host :start (1+ colon)))
                                         ;; Cut off starting slash.
                                         :path (subseq (the string
                                                            (hunchentoot:url-decode
                                                             (hunchentoot:script-name request)
                                                             *default-external-format*))
                                                       1))
                                        :http-method (hunchentoot:request-method request)
                                        :headers (hunchentoot:headers-in request)
                                        :post (hunchentoot:post-parameters request)
                                        :get (hunchentoot:get-parameters request)
                                        :cookies (hunchentoot:cookies-in request)
                                        :remote (hunchentoot:remote-addr request))
                               request))))

(setf hunchentoot:*dispatch-table* (list #'pre-handler))
