#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-postmodern)

(defmacro with-query ((query-form &optional (where 'where) (vars 'vars)) &body body)
  (let ((res (gensym "RESULT")))
    `(let* ((,res ,query-form)
            (,where (car ,res))
            (,vars (cdr ,res)))
       ,@body)))

(defmacro with-collection-existing ((collection) &body body)
  `(let ((,collection (ensure-collection-name ,collection)))
     (handler-case (progn ,@body)
       (cl-postgres-error:syntax-error-or-access-violation (err)
         (when (string= "42P01" (cl-postgres-error::database-error-code err))
           (error 'db:invalid-collection :database *current-db*
                                         :collection ,collection
                                         :message (cl-postgres-error::database-error-message err)))))))

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
    (setf string (prin1-to-string string))
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
