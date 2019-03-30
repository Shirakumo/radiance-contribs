#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(deftype db:id ()
  '(integer 0))

(defun db:ensure-id (id-ish)
  (etypecase id-ish
    (integer id-ish)
    (string (parse-integer id-ish))))

(defun db:collections ()
  (remove-if (lambda (name)
               (and (<= (length "sqlite_") (length name))
                    (string= name "sqlite_" :end1 (length "sqlite_"))))
             (db:iterate "sqlite_master" (db:query (:= 'type "table"))
               (lambda (row) (gethash "name" row))
               :sort '(("name" :ASC)) :accumulate T)))

(defun db:collection-exists-p (collection)
  (ignore-errors
   (ensure-collection-name collection T)))

(defun db:create (collection structure &key indices (if-exists :ignore))
  (let ((collection (ensure-collection-name collection)))
    (flet ((err (msg) (error 'db:invalid-collection :database *current-db* :collection collection :message msg)))
      (let ((query (format NIL "CREATE TABLE \"~a\" (\"_id\" INTEGER PRIMARY KEY AUTOINCREMENT~{, ~a~});"
                           collection (mapcar #'compile-field structure))))
        (when (db:collection-exists-p collection)
          (ecase if-exists
            (:ignore (return-from db:create NIL))
            (:error (error 'db:collection-already-exists :database *current-db* :collection collection))
            (:supersede (db:drop collection))))
        (exec-query query ())
        (dolist (index indices)
          (let ((index (if (listp index) index (list index))))
            (unless (every (lambda (index) (member index `((_id) ,@structure) :key #'car :test #'string-equal)) index)
              (err (format NIL "Index on field ~s requested but it does not exist." index)))
            (exec-query (format NIL "CREATE INDEX \"~a-~a\" ON \"~:*~:*~a\" (~{\"~(~a~)\"~^, ~})" collection index) ())))
        collection))))

(defun compile-field (field)
  (flet ((err (msg) (error 'db:invalid-field :field field :message msg)))
    (destructuring-bind (name type) field
      (unless (valid-name-p name)
        (err "Invalid name, only a-z, - and _ are allowed."))
      (let ((arg (when (listp type) (prog1 (second type) (setf type (first type))))))
        (case type
          ((:INTEGER :ID)
           (format NIL "\"~a\" ~a" (string-downcase name)
                   (ecase arg ((1 2) "SMALLINT") ((3 4) "INTEGER") ((5 6 7 8) "BIGINT") ((NIL) "INTEGER"))))
          (:FLOAT
           (when arg (err "FLOAT cannot accept an argument."))
           (format NIL "\"~a\" DOUBLE PRECISION" (string-downcase name)))
          (:VARCHAR
           (unless arg (err "VARCHAR requires a length argument."))
           (format NIL "\"~a\" VARCHAR(~d)" (string-downcase name) arg))
          (:TEXT
           (when arg (err "TEXT cannot accept an argument."))
           (format NIL "\"~a\" TEXT" (string-downcase name)))
          (T
           (error 'db:invalid-field :field arg)))))))

(defun db:structure (collection)
  (cdr
   (loop for field in (sqlite:execute-to-list *current-con* (format NIL "PRAGMA table_info(\"~a\")" (ensure-collection-name collection T)))
	 collect (destructuring-bind (index name type &rest rest) field
		   (declare (ignore index rest))
		   (list name
			 (cond ((string-equal type "INTEGER")
				:INTEGER)
			       ((string-equal type "SMALLINT")
				(list :INTEGER 2))
			       ((string-equal type "BIGINT")
				(list :INTEGER 8))
			       ((string-equal type "DOUBLE PRECISION")
				:FLOAT)
			       ((string-equal type "TEXT")
				:TEXT)
			       ((string-equal type "VARCHAR" :end1 7)
				(list :VARCHAR (parse-integer (subseq type 8 (1- (length type))))))))))))

(defun db:empty (collection)
  (with-collection-existing (collection)
    (exec-query (format NIL "DELETE FROM ~s;" collection) ())
    (exec-query "VACUUM;" ())
    T))

(defun db:drop (collection)
  (with-collection-existing (collection)
    (exec-query (format NIL "DROP TABLE ~s;" collection) ())
    T))

(defun collect-statement-to-table (statement)
  (loop with table = (make-hash-table :test 'equalp)
        for field in (sqlite:statement-column-names statement)
        for i from 0
        do (setf (gethash field table)
                 (sqlite:statement-column-value statement i))
        finally (return table)))

(defun collecting-iterator (function)
  (lambda (statement)
    (loop collect (funcall function (collect-statement-to-table statement))
          while (sqlite:step-statement statement))))

(defun dropping-iterator (function)
  (lambda (statement)
    (loop do (funcall function (collect-statement-to-table statement))
          while (sqlite:step-statement statement))))

(defun db:iterate (collection query function &key fields skip amount sort accumulate unique)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "SELECT~:[~; DISTINCT~] ~:[*~;~:*~{\"~a\"~^ ~}~] FROM \"~a\""
                                     unique (mapcar #'string-downcase fields) collection)
                             query skip amount sort) query vars)
      (exec-query query vars (if accumulate (collecting-iterator function) (dropping-iterator function))))))

(defun db:select (collection query &key fields skip amount sort unique)
  (db:iterate collection query #'identity :fields fields :skip skip :amount amount :sort sort :unique unique :accumulate T))

(defun db:count (collection query)
  (with-collection-existing (collection)
    (with-query (query where vars)
      (let ((query (format NIL "SELECT COUNT(*) FROM \"~a\" ~a;" collection where)))
        (exec-query query vars (lambda (statement)
                                 (return-from db:count (sqlite:statement-column-value statement 0))))))))

(defun db:insert (collection data)
  (with-collection-existing (collection)
    (let ((query (format NIL "INSERT INTO ~s (~~{\"~~a\"~~^, ~~}) VALUES (~~:*~~{~~*?~~^, ~~});" collection)))
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect (string-downcase field) into fields
                          collect value into values
                          finally (exec-query (format NIL query fields) values))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      (sqlite:last-insert-rowid *current-con*))))

(defun db:remove (collection query &key skip amount sort)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "DELETE FROM ~s WHERE \"_id\" IN (SELECT \"_id\" FROM \"~:*~a\" " collection)
                             query skip amount sort) query vars)
      (exec-query (format NIL "~a );" query) vars)
      T)))

(defun db:update (collection query data &key skip amount sort)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "UPDATE ~s SET ~~{\"~~a\" = ?~~^, ~~} WHERE ROWID IN (SELECT ROWID FROM \"~:*~a\" " collection)
                             query skip amount sort) query vars)
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect value into values
                          collect (string-downcase field) into fields
                          finally (exec-query (format NIL "~a);" (format NIL query fields)) (append values vars)))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      T)))

(defmacro db:with-transaction (() &body body)
  `(sqlite:with-transaction *current-con*
     ,@body))


(define-trigger server-start ()
  (defaulted-config "test.db" :connections "test")
  (defaulted-config "radiance.db" :connections "radiance")
  (db:connect (defaulted-config "radiance" :default))
  (load-pcre))

(define-trigger server-stop ()
  (db:disconnect))
