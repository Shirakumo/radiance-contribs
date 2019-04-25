#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-woo
    (:use #:cl #:radiance)
    (:implements #:server))
(in-package #:i-woo)

(defvar *listeners* (make-hash-table :test 'eql))

(define-trigger server-start ()
  (defaulted-config `(:address "0.0.0.0" :port ,(or (mconfig :radiance-core :port) 8080)) :default)
  (defaulted-config `(:default) :enabled)
  (loop for name in (config :enabled)
        do (apply #'server:start name (config name))))

(define-trigger server-stop ()
  (mapcar #'server:stop (server:listeners)))

(defun server:start (name &key port address)
  (check-type name keyword)
  (when (gethash name *listeners*)
    (error "Server already started."))
  (l:info :server "Starting listener ~a" name)
  (setf (gethash name *listeners*)
        (bt:make-thread
         (lambda ()
           (catch 'stop-server
             (woo:run #'handle-request :port port :address (or address "0.0.0.0"))))))
  (trigger 'server:started name))

(defun server:stop (name)
  (check-type name keyword)
  (let ((listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (when (bt:thread-alive-p listener)
         (bt:interrupt-thread listener (lambda () (throw 'stop-server NIL))))
       (remhash name *listeners*)
       (trigger 'server:stopped name))
      (T
       (error "No such listener found!")))))

(defun server:listeners ()
  (loop for name being the hash-keys of *listeners*
        collect name))

(defun url-decode (string)
  (when (string= string "")
    (return-from url-decode string))
  (let ((vector (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for char = (aref string i)
          do (case char
               (#\%
                (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) vector)
                (incf i 2))
               (#\+
                (vector-push (char-code #\Space) vector))
               (t
                (vector-push (char-code char) vector))))
    (flexi-streams:octets-to-string vector :external-format :utf-8)))

(defun ends-with (end string)
  (and (<= (length end) (length string))
       (string= end string :start2 (- (length string) (length end)))))

(defun parse-get (get)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for pair in (uiop:split-string get :separator '(#\&))
          do (let ((pos (position #\= pair)))
               (when pos
                 (let ((key (url-decode (subseq pair 0 pos)))
                       (val (subseq pair (1+ pos))))
                   (if (ends-with "[]" key)
                       (push val (gethash key table))
                       (setf (gethash key table) val))))))
    table))

(defun starts-with (start string)
  (and (<= (length start) (length string))
       (string= start string :end2 (length start))))

(defun slurp-character-stream (stream)
  "Quickly slurps the stream's contents into an array with fill pointer."
  (declare (type stream stream)
           (optimize speed))
  (with-output-to-string (string)
    (let ((buffer (make-array 4096 :element-type 'character)))
      (loop for bytes = (read-sequence buffer stream)
            do (write-sequence buffer string :start 0 :end bytes)
            while (= bytes 4096)))))

(defun handle-stream-to-string (stream)
  (etypecase stream
    (flexi-streams::vector-input-stream
     (babel:octets-to-string (flexi-streams::vector-stream-vector stream)
                             :start (flexi-streams::vector-stream-index stream)
                             :end (flexi-streams::vector-stream-end stream)))
    (flexi-streams:flexi-stream
     (setf (flexi-streams:flexi-stream-element-type stream) 'character)
     (slurp-character-stream stream))
    (stream
     (let ((type (stream-element-type stream)))
       (cond ((subtypep type '(unsigned-byte 8))
              (slurp-character-stream (flexi-streams:make-flexi-stream stream :element-type 'character :external-format :utf-8)))
             ((subtypep type 'character)
              (slurp-character-stream stream))
             (T (error 'internal-error :message (format NIL "Don't know how to handle stream of element-type ~s" type))))))))

(defun handle-stream-to-file (stream)
  (etypecase stream
    (flexi-streams:vector-stream
     ;; Read to temp file.
     (uiop:with-temporary-file (:stream output
                                :pathname path
                                :prefix "radiance-woo-file"
                                :keep T
                                :direction :output
                                :element-type '(unsigned-byte 8))
       (uiop:copy-stream-to-stream stream output :element-type '(unsigned-byte 8))
       path))
    (stream
     ;; Hopefully already a temp file.
     (pathname stream))
    (pathname
     ;; Convenient.
     stream)))

(defun parse-post (body type length)
  (let ((map (make-hash-table :test 'equalp)))
    (cond
      ((starts-with "multipart/form-data" type)
       ;; Need to parse standard params into table,
       ;; file arguments into (PATH ORIGINAL-FILENAME MIME-TYPE)
       (with-simple-restart (abort "Abort processing more values.")
         (loop for (key body meta headers)
                 in (http-body:parse type length body)
               for content-type = (gethash "content-type" headers)
               for filename = (gethash "filename" meta)
               do (with-simple-restart (skip "Skip processing this value.")
                    (let ((val (if content-type
                                   ;; I don't know if this is a good idea.
                                   (when (string/= filename "")
                                     (list (handle-stream-to-file body)
                                           filename
                                           content-type))
                                   (handle-stream-to-string body))))
                      (when val
                        (if (ends-with "[]" key)
                            (push val (gethash key map))
                            (setf (gethash key map) val)))))
               finally (return map))))
      ((starts-with "application/x-www-form-urlencoded" type)
       (loop for (key . val) in (http-body:parse type length body)
             do (setf (gethash key map) val)
             finally (return map)))
      (T map))))

(defun parse-headers (env)
  ;; woo env is a plist that already provides a hash-table with the headers,
  ;; so no need to build our own.
  (getf env :headers))

(defun parse-cookies (env)
  ;; Cookies are stored as a single key-value pair in woo headers
  ;; and separated by commas.
  (let* ((headers (parse-headers env))
         (cookies-header (and headers (gethash "cookie" headers))))
    (when cookies-header
      (loop with cookies = (make-hash-table :test 'equalp)
            for pair in (uiop:split-string cookies-header :separator '(#\;)) do
              (destructuring-bind (key val)
                  (uiop:split-string pair :separator '(#\=))
                (setf (gethash (string-trim '(#\space) key) cookies)
                      (string-trim '(#\space) val)))
            finally (return cookies)))))

(defun transform-response (request response)
  (list (return-code response)
        (let ((headers (list :content-type (content-type response))))
          (loop for header being the hash-keys of (headers response)
                for value being the hash-values of (headers response)
                do (push value headers)
                   (push (intern (string-upcase header) "KEYWORD") headers))
          (loop for cookie being the hash-keys of (cookies response)
                for value being the hash-values of (cookies response)
                do (push (cookie-header value) headers)
                   (push (intern (string-upcase cookie) "KEYWORD") headers))
          headers)
        (etypecase (data response)
          (string (list (data response)))
          ((array (unsigned-byte 8)) (data response))
          (pathname (data response))
          (null (error 'request-empty :request request)))))

(defun maybe-invoke-restart (restart &optional condition)
  (let ((restart (find-restart restart condition)))
    (when restart (invoke-restart restart))))

;; Handle all the things!
(defun handle-request (env)
  (handler-bind ((error #'handle-condition))
    (restart-case
        (destructuring-bind (&key raw-body request-method server-name server-port request-uri query-string content-type content-length &allow-other-keys) env
          (multiple-value-bind (response request)
              (handler-bind ((error (lambda (err)
                                      (maybe-invoke-restart 'skip err)
                                      (maybe-invoke-restart 'abort err))))
                (request (make-instance 'uri :path (subseq request-uri 1 (position #\? request-uri))
                                             :port server-port
                                             :domains (nreverse (uiop:split-string server-name :separator '(#\.))))
                         :http-method request-method
                         :body-stream raw-body
                         :headers (parse-headers env)
                         :post (parse-post raw-body content-type content-length)
                         :get (parse-get query-string)
                         :cookies (parse-cookies env)
                         :remote (getf env :remote-addr)))
            ;; Clean up temporary files
            (handler-case
                (flet ((handle-var (var)
                         (when (listp var)
                           (uiop:delete-file-if-exists (first var)))))
                  (loop for key being the hash-keys of (post-data request)
                        for val being the hash-values of (post-data request)
                        do (if (ends-with "[]" key)
                               (mapcar #'handle-var val)
                               (handle-var val))))
              (error (err)
                (v:severe :server "Error in cleanup: ~a" err)))
            ;; Present output
            (transform-response request response)))
      (abort ()
        :report "Abort and send back an internal error."
        (error 'internal-error :message "Oh dear.")))))
