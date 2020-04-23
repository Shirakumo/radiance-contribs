#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(defvar *structure-cache*
  (make-hash-table :test 'equal))

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
  (let ((collection (coerce-collection-name collection)))
    (flet ((err (msg) (error 'db:invalid-collection :database *current-db* :collection collection :message msg)))
      (let ((query (format NIL "CREATE TABLE ~s (\"_id\" INTEGER PRIMARY KEY AUTOINCREMENT~{, ~a~});"
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
        ;; Need to cache database structure here for boolean reconstruction later...
        (setf (gethash collection *structure-cache*) structure)
        collection))))

(defun compile-field (field)
  (flet ((err (msg) (error 'db:invalid-field :field field :message msg)))
    (destructuring-bind (name type) field
      (unless (valid-name-p name)
        (err "Invalid name, only a-z, - and _ are allowed."))
      (let ((arg (when (listp type) (prog1 (second type) (setf type (first type))))))
        (case type
          (:ID
           (format NIL "~s INTEGER~@[ REFERENCES ~a(\"_id\")~]"
                   (string-downcase name) (when arg (ensure-collection-name arg))))
          (:INTEGER
           (format NIL "~s ~a" (string-downcase name)
                   (ecase arg (1 "TINYINT") (2 "SMALLINT") (3 "MEDIUMINT") (4 "INTEGER") ((5 6 7 8) "BIGINT") ((NIL) "INTEGER"))))
          (:FLOAT
           (when arg (err "FLOAT cannot accept an argument."))
           (format NIL "~s DOUBLE PRECISION" (string-downcase name)))
          (:VARCHAR
           (unless arg (err "VARCHAR requires a length argument."))
           (format NIL "~s VARCHAR(~d)" (string-downcase name) arg))
          (:TEXT
           (when arg (err "TEXT cannot accept an argument."))
           (format NIL "~s TEXT" (string-downcase name)))
          (:BOOLEAN
           (when arg (err "BOOLEAN cannot accept an argument."))
           (format NIL "~s BOOLEAN" (string-downcase name)))
          (T
           (error 'db:invalid-field :field arg)))))))

(defun db:structure (collection)
  (let ((collection (ensure-collection-name collection T)))
    (or (gethash collection *structure-cache*)
        (flet ((reconstruct-field (field)
                 (destructuring-bind (index name type &rest rest) field
                   (declare (ignore index rest))
                   (list name
                         (cond ((string-equal type "INTEGER")
                                :INTEGER)
                               ((string-equal type "TINYINT")
                                (list :INTEGER 1))
                               ((string-equal type "SMALLINT")
                                (list :INTEGER 2))
                               ((string-equal type "MEDIUMINT")
                                (list :INTEGER 3))
                               ((string-equal type "BIGINT")
                                (list :INTEGER 8))
                               ((string-equal type "DOUBLE PRECISION")
                                :FLOAT)
                               ((string-equal type "TEXT")
                                :TEXT)
                               ((string-equal type "VARCHAR" :end1 7)
                                (list :VARCHAR (parse-integer (subseq type 8 (1- (length type))))))
                               ((string-equal type "BOOLEAN")
                                :BOOLEAN))))))
          (setf (gethash collection *structure-cache*)
                (cdr (mapcar #'reconstruct-field (sqlite:execute-to-list *current-con* (format NIL "PRAGMA table_info(~a)" collection)))))))))

(defun db:empty (collection)
  (with-collection-existing (collection)
    (exec-query (format NIL "DELETE FROM ~a;" collection) ())
    (exec-query "VACUUM;" ())
    T))

(defun db:drop (collection)
  (with-collection-existing (collection)
    (exec-query (format NIL "DROP TABLE ~a;" collection) ())
    T))

(defun collect-statement-to-table (structure statement)
  (loop with table = (make-hash-table :test 'equalp)
        for field in (sqlite:statement-column-names statement)
        for i from 0
        for value = (sqlite:statement-column-value statement i)
        for definition = (find field structure :key #'first :test #'string-equal)
        do (setf (gethash field table)
                 (if (eql :boolean (second definition))
                     (= 1 value)
                     value))
        finally (return table)))

(defun collecting-iterator (structure function)
  (lambda (statement)
    (loop collect (funcall function (collect-statement-to-table structure statement))
          while (sqlite:step-statement statement))))

(defun dropping-iterator (structure function)
  (lambda (statement)
    (loop do (funcall function (collect-statement-to-table structure statement))
          while (sqlite:step-statement statement))))

(defun db:iterate (collection query function &key fields skip amount sort accumulate unique)
  (let ((structure (db:structure collection)))
    (with-collection-existing (collection)
      (with-query ((make-query (format NIL "SELECT~:[~; DISTINCT~] ~:[*~;~:*~{~s~^ ~}~] FROM ~a"
                                       unique (mapcar #'string-downcase fields) collection)
                               query skip amount sort) query vars)
        (exec-query query vars (if accumulate (collecting-iterator structure function) (dropping-iterator structure function)))))))

(defun db:select (collection query &key fields skip amount sort unique)
  (db:iterate collection query #'identity :fields fields :skip skip :amount amount :sort sort :unique unique :accumulate T))

(defun db:count (collection query)
  (with-collection-existing (collection)
    (with-query (query where vars)
      (let ((query (format NIL "SELECT COUNT(*) FROM ~a ~a;" collection where)))
        (exec-query query vars (lambda (statement)
                                 (return-from db:count (sqlite:statement-column-value statement 0))))))))

(defun db:insert (collection data)
  (with-collection-existing (collection)
    (let ((query (format NIL "INSERT INTO ~a (~~{~~s~~^, ~~}) VALUES (~~:*~~{~~*?~~^, ~~});" collection)))
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect (case value ((T) 1) ((NIL) 0) (T value)) into values
                          collect (string-downcase field) into fields
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
    (with-query ((make-query (format NIL "DELETE FROM ~a WHERE \"_id\" IN (SELECT \"_id\" FROM ~:*~a " collection)
                             query skip amount sort) query vars)
      (exec-query (format NIL "~a );" query) vars)
      T)))

(defun db:update (collection query data &key skip amount sort)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "UPDATE ~a SET ~~{~~s = ?~~^, ~~} WHERE ROWID IN (SELECT ROWID FROM ~:*~a " collection)
                             query skip amount sort) query vars)
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect (case value ((T) 1) ((NIL) 0) (T value)) into values
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
