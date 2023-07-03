(in-package #:modularize-user)
(define-module simple-cache
  (:use #:cl #:radiance)
  (:implements #:cache))
(in-package #:simple-cache)

(defvar *cache-directory* (make-pathname :name NIL :type NIL :defaults
                                         (merge-pathnames "cache/" (mconfig-pathname #.*package*))))
(defvar *caches* (make-hash-table :test 'eql))

(define-trigger startup ()
  (ensure-directories-exist *cache-directory*))

(define-trigger shutdown ()
  (uiop:delete-directory-tree *cache-directory* :validate (constantly T)))

(defun (setf cache::builder) (builder-func name)
  (setf (gethash name *caches*) builder-func))

(defun cache::builder (name)
  (gethash name *caches*))

(defun cache::file (symbol &optional variant)
  (merge-pathnames (format NIL "~a/~a~@[/~a~].cache" (package-name (symbol-package symbol)) (symbol-name symbol) variant)
                   *cache-directory*))

(defun cache::exists (name &optional variant)
  (probe-file (cache::file name variant)))

(defun cache:get (name &optional variant)
  (when (cache::exists name variant)
    (alexandria:read-file-into-byte-vector
     (cache::file name variant))))

(defun cache:renew (name &optional variant)
  (when (cache::exists name variant)
    (delete-file (cache::file name variant))))

(defun cache::output (result name &optional variant)
  (let ((file (cache::file name variant)))
    (ensure-directories-exist file)
    (etypecase result
      (string
       (with-open-file (stream file :direction :output :if-exists :supersede)
         (write-string result stream)))
      ((array (unsigned-byte 8))
       (with-open-file (stream file :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
         (write-sequence result stream)))))
  result)

(defmacro cache:with-cache ((name &optional variant) test &body request-generator)
  (let ((thunk (gensym "THUNK")) (variantg (gensym "VARIANT")))
    `(flet ((,thunk ()
              ,@request-generator))
       (let ((,variantg ,variant))
         (if ,test
             (cache::output (,thunk) ',name ,variantg)
             (or (cache:get ',name ,variantg)
                 (cache::output (,thunk) ',name ,variantg)))))))
