#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(defmacro with-collection-existing ((collection) &body body)
  `(let ((,collection (ensure-collection-name ,collection)))
     (handler-bind
         ((sqlite:sqlite-error
            (lambda (err)
              ;; Gross, but what can you do.
              (when (and (<= 13 (length (sqlite:sqlite-error-message err)))
                         (string= "no such table" (sqlite:sqlite-error-message err) :end2 13))
                (error 'db:invalid-collection :database *current-db* 
                                              :collection ,collection
                                              :message (sqlite:sqlite-error-message err))))))
       ,@body)))

(defun valid-name-p (name)
  (loop for char across (string-downcase name)
        always (or (alpha-char-p char) (char= char #\_) (char= char #\-))))

(defun ensure-collection-name (name &optional check-exists)
  (let ((string (etypecase name
                  (string name)
                  (symbol (format NIL "~a/~a" (symbol-package name) (symbol-name name))))))
    (unless (valid-name-p collection)
      (error 'database-invalid-collection :collection collection :message "Invalid name, only a-z, - and _ are allowed."))
    (when check-exists
      (when (= 0 (db:count 'sqlite_master (db:query (:and (:= 'type "table") (:= 'name string)))))
        (error 'database-invalid-collection :collection collection :message "Collection does not exist on database.")))
    string))
