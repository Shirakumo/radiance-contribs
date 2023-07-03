(in-package #:modularize-user)
(define-module #:simple-rate
  (:use #:cl #:radiance)
  (:implements #:rate))
(in-package #:simple-rate)

(defvar *rates* (make-hash-table))

(define-trigger db:connected ()
  ;; See http://stackoverflow.com/a/7477384/743237 for the IP length
  (db:create 'tracking '((limit (:varchar 64))
                         (time (:integer 5))
                         (amount :integer)
                         (ip (:varchar 45)))
             :indices '(limit ip)))

(defclass limit ()
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (timeout :initarg :timeout :initform 60 :accessor timeout)
   (amount :initarg :amount :initform 1 :accessor amount)
   (exceeded :initarg :exceeded :initform (lambda (s) (error "Please wait ~d second~:p." s)) :accessor exceeded)))

(defun db-limit-name (name)
  (let ((name (etypecase name
                (string name)
                (symbol (format NIL "~a:~a"
                                (package-name (symbol-package name))
                                (symbol-name name))))))
    (if (<= (length name) 64)
        name
        (error "Rate name too long."))))

(defun limit (name)
  (gethash name *rates*))

(defun (setf limit) (rate name)
  (setf (gethash name *rates*)
        rate))

(defmacro rate:define-limit (name (time-left &key (timeout 60) (limit 1)) &body on-limit-exceeded)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (limit ',name)
           (make-instance
            'limit
            :name ,(db-limit-name name)
            :amount ,limit
            :timeout ,timeout
            :exceeded #'(lambda (,time-left) ,@on-limit-exceeded)))))

(defun rate:left (limit &key (ip (remote *request*)))
  (let* ((limit (limit limit))
         (tracking (dm:get-one 'tracking (db:query (:and (:= 'limit (name limit))
                                                         (:= 'ip ip))))))
    (if tracking
        (values (dm:field tracking "amount")
                (- (+ (dm:field tracking "time")
                      (timeout limit))
                   (get-universal-time)))
        (values (amount limit)
                (timeout limit)))))

(defun rate::tax-rate (limit &key (ip (remote *request*)))
  (let* ((limit (limit limit))
         (tracking (dm:get-one 'tracking (db:query (:and (:= 'limit (name limit))
                                                         (:= 'ip ip))))))
    (cond (tracking
           ;; If we're out of attempts, but the time has also passed, reset.
           (when (and (<= 0 (dm:field tracking "amount"))
                      (<= (+ (dm:field tracking "time") (timeout limit)) (get-universal-time)))
             (setf (dm:field tracking "amount") (amount limit)))
           ;; Tax it.
           (decf (dm:field tracking "amount"))
           (setf (dm:field tracking "time") (get-universal-time))
           (dm:save tracking))
          (T
           (db:insert 'tracking `((limit . ,(name limit))
                                  (time . ,(get-universal-time))
                                  (amount . ,(amount limit))
                                  (ip . ,ip)))))))

(defmacro rate:with-limitation ((limit) &body body)
  (assert (limit limit) () "No such limit ~s." limit)
  (let ((amount (gensym "AMOUNT"))
        (timeout (gensym "TIMEOUT")))
    `(multiple-value-bind (,amount ,timeout) (rate:left ',limit)
       (if (and (<= ,amount 0) (< 0 ,timeout))
           (funcall (exceeded (limit ',limit)) ,timeout)
           (progn
             (rate::tax-rate ',limit)
             ,@body)))))
