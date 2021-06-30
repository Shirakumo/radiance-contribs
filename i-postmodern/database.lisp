#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-postmodern)

;; TODO:
;; Optimize the shit out of this with compiler macros
;; maybe even triggers to compile all prepared statements on db connect or some shit

;; TODO:
;; Implement closer erroring details of spec. (invalid fields, etc)

(deftype db:id ()
  '(integer 0))

(defun db:ensure-id (id-ish)
  (etypecase id-ish
    ((integer 0) id-ish)
    (string (db:ensure-id (parse-integer id-ish)))))

(defun db:collections ()
  (with-connection
    (postmodern:list-tables T)))

(defun %table-exists-p (table)
  (postmodern:query (format NIL "select 1 from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '~a'"
                            (subseq table 1 (1- (length table))))))

(defun db:collection-exists-p (collection)
  (with-connection
    (%table-exists-p (ensure-collection-name collection))))

(defun db:create (collection structure &key indices (if-exists :ignore))
  (flet ((err (msg) (error 'db:invalid-collection :database *current-db* :collection collection :message msg)))
    (unless structure (err "Structure cannot be empty."))
    (let* ((collection (ensure-collection-name collection))
           (query (format NIL "CREATE TABLE ~a (\"_id\" INTEGER PRIMARY KEY DEFAULT nextval('~a'), ~{~a~^, ~});"
                          collection (collection-sequence collection) (mapcar #'compile-field structure))))
      (with-connection
        (when (%table-exists-p collection)
          (ecase if-exists
            (:ignore (return-from db:create NIL))
            (:error (error 'db:collection-already-exists :database *current-db* :collection collection))))
        (postmodern:query (format NIL "CREATE SEQUENCE IF NOT EXISTS ~a;" (collection-sequence collection)))
        (postmodern:query query)
        (postmodern:query (format NIL "CREATE INDEX ON ~a (\"_id\")" collection))
        (dolist (index indices)
          (let ((index (if (listp index) index (list index))))
            (unless (every (lambda (index) (member index `((_id) ,@structure) :key #'car :test #'string-equal)) index)
              (err (format NIL "Index on field ~s requested but it does not exist." index)))
            (postmodern:query (format NIL "CREATE INDEX ON ~a (~{\"~(~a~)\"~^, ~})"
                                      collection index)))))
      T)))

(defun compile-field (field)
  (flet ((err (msg) (error 'db:invalid-field :database *current-db* :field field :message msg)))
    (destructuring-bind (name type) field
      (let ((arg (when (listp type) (prog1 (second type) (setf type (first type)))))
            (name (string-downcase name)))
        (unless (valid-name-p name)
          (err "Invalid name, only a-z, - and _ are allowed."))
        (ecase type
          (:ID
           (format NIL "~s INTEGER~@[ REFERENCES ~a(\"_id\")~]"
                   name (when arg (ensure-collection-name arg))))
          (:BOOLEAN
           (when arg (err "BOOLEAN cannot accept an argument."))
           (format NIL "~s BOOLEAN" name))
          (:INTEGER
           (format NIL "~s ~a" name
                   (ecase arg ((1 2) "SMALLINT") ((3 4) "INTEGER") ((5 6 7 8) "BIGINT") ((NIL) "INTEGER"))))
          (:FLOAT
           (when arg (err "FLOAT cannot accept an argument."))
           (format NIL "~s DOUBLE PRECISION" name))
          (:BYTE
           (unless arg (err "BYTE requires a length argument."))
           (format NIL "~s BYTEA(~d)" name arg))
          (:CHARACTER
           (error "CURRENTLY NOT SUPPORTED."))
          (:VARCHAR
           (unless arg (err "VARCHAR requires a length argument."))
           (format NIL "~s VARCHAR(~d)" name arg))
          (:TEXT
           (when arg (err "TEXT cannot accept an argument."))
           (format NIL "~s TEXT" name)))))))

(defun db:structure (collection)
  (with-connection
      (let ((table (ensure-collection-name collection T)))
        (rest 
         (mapcar (lambda (column)
                   (destructuring-bind (name type size) column
                     (list name (cond ((string= type "boolean")
                                       :BOOLEAN)
                                      ((string= type "integer")
                                       :INTEGER)
                                      ((string= type "smallint")
                                       (list :INTEGER 2))
                                      ((string= type "bigint")
                                       (list :INTEGER 8))
                                      ((string= type "double precision")
                                       :FLOAT)
                                      ((string= type "character varying")
                                       (list :VARCHAR size))
                                      ((string= type "text")
                                       :TEXT)))))
                 (postmodern:query (format NIL "select column_name, data_type, character_maximum_length from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '~a'"
                                           (subseq table 1 (1- (length table))))))))))

(defun db:empty (collection)
  (with-collection-existing (collection)
    (with-connection
      (postmodern:query (format NIL "TRUNCATE TABLE ~a CASCADE;" collection))
      T)))

(defun db:drop (collection)
  (with-collection-existing (collection)
    (with-connection
      (postmodern:query (format NIL "DROP TABLE ~a CASCADE;" collection))
      (postmodern:query (format NIL "DROP SEQUENCE ~a CASCADE;" (collection-sequence collection)))
      T)))

(defun collecting-iterator (function)
  (cl-postgres:row-reader (fields)
    (loop while (cl-postgres:next-row)
          for table = (let ((table (make-hash-table :test 'equalp)))
                        (loop for field across fields
                              for name = (cl-postgres:field-name field)
                              for value = (cl-postgres:next-field field)
                              do (setf (gethash name table) (unless (eql value :null) value)))
                        table)
          collect (funcall function table))))

(defun dropping-iterator (function)
  (cl-postgres:row-reader (fields)
    (loop while (cl-postgres:next-row)
          for table = (let ((table (make-hash-table :test 'equalp)))
                        (loop for field across fields
                              for name = (cl-postgres:field-name field)
                              for value = (cl-postgres:next-field field)
                              do (setf (gethash name table) (unless (eql value :null) value)))
                        table)
          do (funcall function table))))

(defun db:iterate (collection query function &key fields skip amount sort accumulate unique)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "SELECT~:[~; DISTINCT~] ~:[*~;~:*~{~s~^ ~}~] FROM ~a"
                                     unique (mapcar #'string-downcase fields) collection)
                             query skip amount sort) query vars)
      (exec-query query vars (if accumulate (collecting-iterator function) (dropping-iterator function))))))

(defun db:select (collection query &key fields skip amount sort unique)
  (db:iterate collection query #'identity
              :fields fields
              :skip skip
              :amount amount
              :sort sort
              :unique unique
              :accumulate T))

(defun db:count (collection query)
  (with-collection-existing (collection)
    (with-query (query where vars)
      (let ((query (format NIL "SELECT COUNT(*) AS c FROM ~a ~a;" collection where)))
        (car (exec-query query vars (collecting-iterator #'(lambda (ta) (gethash "c" ta)))))))))

(defun db:insert (collection data)
  (with-collection-existing (collection)
    (let ((query (format NIL "INSERT INTO ~a (~~{\"~~a\"~~^, ~~}) VALUES (~~{$~~a~~^, ~~}) RETURNING \"_id\";" collection)))
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters 
                          for i from 1
                          collect (string-downcase field) into fields
                          collect (or value :null) into values
                          collect i into nums
                          finally (return (car (exec-query (format NIL query fields nums) values
                                                           (collecting-iterator #'(lambda (ta) (gethash "_id" ta)))))))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data)))))))

(defun db:remove (collection query &key skip amount sort)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "DELETE FROM ~a WHERE ctid IN (SELECT ctid FROM ~:*~a " collection)
                             query skip amount sort) query vars)
      (exec-query (format NIL "~a );" query) vars)
      T)))

(defun %field-clause (s a c p)
  (declare (ignore c p))
  (format s "~s = $~a" (car a) (cdr a)))

(defun db:update (collection query data &key skip amount sort)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "UPDATE ~a SET ~~{~~/i-postmodern::%field-clause/~~^, ~~} WHERE ctid IN (SELECT ctid FROM ~:*~a "
                                     collection)
                             query skip amount sort) query vars)
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          for i from (1+ (length vars))
                          collect (or value :null) into values
                          collect (cons (string-downcase field) i) into fields
                          finally (exec-query (format NIL "~a );" (format NIL query fields)) (append vars values)))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      T)))

(defmacro db:with-transaction (() &body body)
  `(with-connection
     (postmodern:with-logical-transaction ()
       ,@body)))


(define-trigger server-start ()
  (db:connect (defaulted-config "radiance" :default)))

(define-trigger server-stop ()
  (db:disconnect))
