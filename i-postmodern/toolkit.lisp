(in-package #:i-postmodern)

(define-condition db::postgres-error (db:condition error)
  ((original-error :initarg :original-error :reader original-error)))

(defmacro with-query ((query-form &optional (where 'where) (vars 'vars)) &body body)
  (let ((res (gensym "RESULT")))
    `(let* ((,res ,query-form)
            (,where (car ,res))
            (,vars (cdr ,res)))
       ,@body)))

(defmacro with-collection-existing ((collection) &body body)
  `(let ((,collection (ensure-collection-name ,collection)))
     (handler-bind ((cl-postgres:database-error
                       (lambda (err)
                         (flet ((err (type &rest args)
                                  (apply #'error type
                                         :database *current-db*
                                         :message (cl-postgres-error::database-error-message err)
                                         args)))
                           (cond ((string= "42P01" (cl-postgres-error::database-error-code err))
                                  (err 'db:invalid-collection :collection ,collection))
                                 ((string= "42703" (cl-postgres-error::database-error-code err))
                                  (err 'db:invalid-field :field NIL))
                                 (T
                                  (err 'db::postgres-error :original-error err)))))))
       ,@body)))

(defun valid-name-p (name)
  (loop for char across name
        always (find char "-_/abcdefghijklmnopqrstuvwxyz0123456789" :test #'char-equal)))

(defun ensure-collection-name (collection &optional check-exists)
  (let ((string (etypecase collection
                  (symbol (format NIL "~a/~a"
                                  (package-name (symbol-package collection)) (symbol-name collection)))
                  (string collection)
                  (join (return-from ensure-collection-name
                          (join-string collection))))))
    (unless (valid-name-p string)
      (error 'db:invalid-collection :database *current-db*
                                    :collection collection
                                    :message "Invalid name, only a-z, - and _ are allowed."))
    (setf string (with-output-to-string (output)
                   (write-char #\" output)
                   (write-string string output)
                   (write-char #\" output)))
    (when check-exists
      (call-with-connection
       (lambda ()
         (unless (%table-exists-p string)
           (error 'db:invalid-collection :database *current-db*
                                         :collection collection
                                         :message "Collection does not exist on database.")))))
    string))

(defun collection-sequence (collection)
  (with-output-to-string (out)
    (write-sequence collection out :end (1- (length collection)))
    (write-sequence "/ID-SEQ\"" out)))
