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
  (loop for char across (string name)
        always (find char "-_/abcdefghijklmnopqrstuvwxyz0123456789" :test #'char-equal)))

(defun coerce-collection-name (name)
  (let ((collection (etypecase name
                      (string name)
                      (symbol (format NIL "~a/~a"
                                      (package-name (symbol-package name)) (symbol-name name))))))
    (unless (valid-name-p collection)
      (error 'db:invalid-collection :collection collection :message "Invalid name, only a-z, - and _ are allowed."))
    collection))

(defun ensure-collection-name (name &optional check-exists)
  (if (typep name 'join)
      (join-string name)
      (let ((collection (coerce-collection-name name)))
        (when check-exists
          (when (= 0 (db:count "sqlite_master" (db:query (:and (:= 'type "table") (:= 'name collection)))))
            (error 'db:invalid-collection :collection collection :message "Collection does not exist on database.")))
        (prin1-to-string collection))))
