#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:r-clip)

(defun parse-uri* (string)
  (if (char= #\< (aref string 0))
      (multiple-value-bind (resource-string i)
          (parse-resource-string string)
        (resolve-resource resource-string (parse-arglist string i)))
      (multiple-value-bind (pattern-string i)
          (parse-pattern-string string)
        (resolve-pattern pattern-string (parse-arglist string i)))))

(defun resolve-uri (string &rest args)
  (resolve-pattern (parse-pattern-string string) args))

(defun parse-arglist (string &optional (start 0))
  (loop while (< start (length string))
        collect (multiple-value-bind (arg j) (read-from-string string T NIL :start start)
                  (setf start j)
                  (clip:resolve-value arg))))

(defun parse-pattern-string (string)
  (let ((i 0))
    (values (with-output-to-string (out)
              (loop while (< i (length string))
                    for char = (aref string i)
                    do (case char
                         (#\\ (incf i) (write-char (aref string i) out))
                         (#\  (return))
                         (T (write-char char out)))
                       (incf i)))
            i)))

(defun parse-resource-string (string)
  (let ((i 0))
    (values (with-output-to-string (out)
              (loop while (< i (length string))
                    for char = (aref string i)
                    do (write-char char out)
                       (incf i)
                    until (char= char #\>)))
            i)))

(defun read-arg (in args)
  (let ((num (with-output-to-string (buf)
               (loop for char = (read-char in NIL)
                     do (case char
                          ((NIL) (error "Incomplete placeholder."))
                          (#\} (return))
                          (T (write-char char buf)))))))
    (nth (parse-integer num) args)))

(defun resolve-resource (resource-string args)
  (with-input-from-string (in resource-string :start 1)
    (let ((parts)
          (buf (make-string-output-stream)))
      (loop for char = (read-char in NIL)
            do (case char
                 ((NIL) (error "Unexpected end of resource. Missing closing > character."))
                 (#\> (push (get-output-stream-string buf) parts) (return))
                 (#\\ (write-char (read-char in) buf))
                 (#\{ (push (read-arg in args) parts))
                 (#\  (push (get-output-stream-string buf) parts))
                 (T (write-char char buf))))
      (destructuring-bind (module type &rest args) (nreverse parts)
        (multiple-value-bind (uri query fragment)
            (apply #'resource (string-upcase module) type args)
          (uri-to-url uri
                      :representation :external
                      :query query
                      :fragment fragment))))))

(defun resolve-pattern (pattern-string args)
  (multiple-value-bind (domains port path query fragment) (destructure-pattern pattern-string args)
    (uri-to-url (make-instance 'uri
                               :domains domains
                               :port port
                               :path path)
                :representation :external
                :query query
                :fragment fragment)))

(defun destructure-pattern (string args)
  (with-input-from-string (in string)
    (let ((domains) (port) (path) (query) (fragment)
          (buf (make-string-output-stream)) (key))
      (macrolet ((with-parsing (finally &body clauses)
                   `(unwind-protect
                         (loop for char = (read-char in NIL)
                               do (case char
                                    ,@clauses
                                    (#\\ (write-char (read-char in) buf))
                                    (#\{ (let ((arg (read-arg in args)))
                                           (when arg (princ arg buf))))
                                    (T (write-char char buf))))
                      ,finally)))
        (tagbody
         read-subdomain
           (with-parsing (let ((string (get-output-stream-string buf)))
                           (when (string/= string "")
                             (push string domains)))
             ((NIL) (error "Path part is missing from pattern."))
             (#\. (go read-subdomain))
             (#\: (go read-port))
             (#\/ (go read-path)))
         read-port
           (with-parsing (setf port (parse-integer (get-output-stream-string buf)))
             ((NIL) (error "Path part is missing from pattern."))
             (#\/ (go read-path)))
         read-path
           (with-parsing (setf path (get-output-stream-string buf))
             ((NIL) (go end))
             (#\? (go read-query))
             (#\# (go read-fragment)))
         read-query
           (with-parsing (setf key (get-output-stream-string buf))
             ((NIL) (go read-value))
             (#\# (go read-fragment))
             (#\& (unread-char #\& in) (go read-value))
             (#\= (go read-value)))
         read-value
           (with-parsing (push (cons key (get-output-stream-string buf)) query)
             ((NIL) (go end))
             (#\# (go read-fragment))
             (#\& (go read-query)))
         read-fragment
           (with-parsing (setf fragment (get-output-stream-string buf))
             ((NIL) (go end)))
         end))
      (values domains port path query fragment))))
