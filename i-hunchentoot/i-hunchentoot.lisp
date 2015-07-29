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

(defun whenthen (var func)
  (when var (funcall func var)))

(define-trigger server-start ()
  (loop for config in (config-tree :server :instances)
        do (server:start (gethash :port config)
                         :address (gethash :address config)
                         :ssl-key (whenthen (gethash :ssl-key config) #'data-file)
                         :ssl-cert (whenthen (gethash :ssl-cert config) #'data-file)
                         :ssl-pass (gethash :ssl-pass config))))

(define-trigger server-stop ()
  (loop for name being the hash-keys of *listeners*
        do (let* ((pos (position #\: name))
                  (port (parse-integer (subseq name (1+ pos))))
                  (address (subseq name 0 pos)))
             (server:stop port (unless (string= address "NIL") address)))))

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

(defun post-handler (response request)
  (declare (optimize (speed 3)))
  (handler-bind
      ((error #'handle-condition))
    (l:trace :server "Post-process: ~a" response)
    ;; Process attributes
    (setf (hunchentoot:return-code*) (return-code response)
          (hunchentoot:content-type*) (content-type response))
    (maphash #'(lambda (key val) (declare (ignore key)) (set-real-cookie val)) (cookies response))
    (maphash #'(lambda (key val) (setf (hunchentoot:header-out (hunchentoot:url-encode key))
                                       (hunchentoot:url-encode val))) (headers response))
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
