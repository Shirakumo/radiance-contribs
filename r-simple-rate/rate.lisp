#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-rate
  (:use #:cl #:radiance)
  (:implements #:rate))
(in-package #:simple-rate)

(defvar *rates* (make-hash-table))

(define-trigger db:connected ()
  ;; See http://stackoverflow.com/a/7477384/743237 for the IP length
  (db:create 'simple-rates '((rate (:varchar 64))
                             (time (:integer 5))
                             (limit :integer)
                             (ip (:varchar 45)))
             :indices '(rate (rate ip))))

(defclass rate ()
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (timeout :initarg :timeout :initform 60 :accessor timeout)
   (limit :initarg :limit :initform 1 :accessor limit)
   (exceeded :initarg :exceeded :initform #'(lambda (limit) (error "Please wait ~d seconds." limit)) :accessor exceeded)))

(defun db-rate-name (name)
  (let ((name (etypecase name
                (string name)
                (symbol (format NIL "~a:~a"
                                (package-name (symbol-package name))
                                (symbol-name name))))))
    (if (<= (length name) 64)
        name
        (error "Rate name too long."))))

(defun rate (name)
  (gethash name *rates*))

(defun (setf rate) (rate name)
  (setf (gethash name *rates*)
        rate))

(defmacro rate:define-limit (name (time-left &key (timeout 60) (limit 1)) &body on-limit-exceeded)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (rate ',name)
           (make-instance
            'rate
            :name ,(db-rate-name name)
            :limit ,limit
            :timeout ,timeout
            :exceeded #'(lambda (,time-left) ,@on-limit-exceeded)))))

(defun rate:left (rate &key (ip (remote *request*)))
  (let* ((rate (rate rate))
         (limit (dm:get-one 'simple-rates (db:query (:and (:= 'rate (name rate))
                                                          (:= 'ip ip))))))
    (if limit
        (values (dm:field limit "limit")
                (- (+ (dm:field limit "time")
                      (timeout rate))
                   (get-universal-time)))
        (values (limit rate)
                (timeout rate)))))

(defun rate::tax-rate (rate &key (ip (remote *request*)))
  (let* ((rate (rate rate))
         (limit (dm:get-one 'simple-rates (db:query (:and (:= 'rate (name rate))
                                                          (:= 'ip ip))))))
    (if limit
        (progn
          ;; If we're out of attempts, but the time has also passed, reset.
          (when (and (<= 0 (dm:field limit "limit"))
                     (<= (+ (dm:field limit "time") (timeout rate)) (get-universal-time)))
            (setf (dm:field limit "limit") (limit rate)))
          ;; Tax it.
          (decf (dm:field limit "limit"))
          (setf (dm:field limit "time") (get-universal-time))
          (dm:save limit))

        (progn
          (db:insert 'simple-rates `((rate . ,(name rate))
                                     (time . ,(get-universal-time))
                                     (limit . ,(limit rate))
                                     (ip . ,ip)))))))

(defmacro rate:with-limitation ((rate) &body body)
  (assert (rate rate) () "No such rate ~s." rate)
  (let ((amount (gensym "AMOUNT"))
        (timeout (gensym "TIMEOUT")))
    `(multiple-value-bind (,amount ,timeout) (rate:left ',rate)
       (if (and (<= ,amount 0) (< 0 ,timeout))
           (funcall (exceeded (rate ',rate)) ,timeout)
           (progn
             (rate::tax-rate ',rate)
             ,@body)))))
