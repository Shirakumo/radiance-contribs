#|
 This file is a part of Radiance
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-lambdalite)

(defvar *db-name* NIL)

(defun make-row-id (collection)
  (lambdalite:with-tx
    (let ((id))
      (lambdalite:update
       :rowid
       (lambda (row) (string= collection (getf row :/collection)))
       (lambda (row) (setf id (incf (getf row :/id))) row))
      (unless id
        (setf id 0)
        (lambdalite:insert
         :rowid
         `(:/collection ,(string collection)
           :/id ,id)))
      id)))

;; Override to fix bug in pathname serialisation.
(defun lambdalite::make-filename (name)
  (with-output-to-string (out)
    (loop for c across (string name)
          do (when (find c ":;[]{}*?\\")
               (write-char #\\ out))
             (write-char (char-downcase c) out))))

(defun valid-name-p (name)
  (loop for char across name
        always (find char "-_/abcdefghijklmnopqrstuvwxyz0123456789" :test #'char-equal)))

(defun ensure-collection (thing)
  (typecase thing
    (string
     (unless (valid-name-p thing)
       (error 'db:invalid-collection :collection thing))
     (intern (map 'string (lambda (c) (if (char= c #\/)
                                          #+windows #\;
                                          #-windows #\:
                                          c)) thing) :keyword))
    (symbol (ensure-collection
             (format NIL "~a/~a"
                     (package-name (symbol-package thing)) (symbol-name thing))))))

(defun ensure-field (thing)
  (typecase thing
    (keyword
     (if (char= #\/ (char (symbol-name thing) 0))
         thing
         (intern (format NIL "/~:@(~a~)" thing) "KEYWORD")))
    (T (intern (format NIL "/~:@(~a~)" thing) "KEYWORD"))))

(defmacro with-table-change ((collection rows) &body body)
  ;; Oh boy we get to reimplement this ourselves because otherwise
  ;; we can't do the sorting and limiting.
  (let ((name (gensym "NAME")))
    `(let ((,name (ensure-collection ,collection)))
       (lambdalite::with-lock
         (when lambdalite::*tx*
           (let ((tmp-name (cdr (assoc ,name lambdalite::*tx-modified-list*)))) 
             (unless tmp-name
               (setf tmp-name (lambdalite::clone-temporary ,name))
               (push (cons ,name tmp-name) lambdalite::*tx-modified-list*)
               (setf ,name tmp-name))
             (setf ,name tmp-name)))
         (let ((,rows (gethash ,name lambdalite::*db*)))
           (setf (gethash ,name lambdalite::*db*)
                 (progn
                   ,@body))
           (unless lambdalite::*tx* (lambdalite::persist ,name)))))))

(deftype db:id ()
  '(integer 0))

(defun db:ensure-id (id-ish)
  (etypecase id-ish
    (integer id-ish)
    (string (parse-integer id-ish))))

(defun db:connect (database-name)
  (with-simple-restart (skip "Skip connecting.")
    (let ((conn (config :connections database-name)))
      (unless conn (error 'db:connection-failed :database database-name
                                                :message "No such connection found."))
      (when lambdalite::*db*
        (warn 'db:connection-already-open :database database-name)
        (db:disconnect))
      ;; Spec restarts for already open.
      (l:info :database "Connecting ~a ~a" database-name conn)
      (lambdalite:load-db
       :path (merge-pathnames
              (etypecase conn
                (pathname conn)
                (string (uiop:parse-native-namestring conn)))
              (mconfig-pathname #.*package*)))
      (setf *db-name* database-name)
      (trigger 'db:connected database-name))))

(defun db:disconnect ()
  (let ((database-name *db-name*))
    (l:info :database "Disconnecting ~a" database-name)
    (setf lambdalite::*db* NIL
          lambdalite::*db-path* NIL
          *db-name* NIL)
    (trigger 'db:disconnected database-name)))

(defun db:connected-p ()
  (not (null lambdalite::*db*)))

(defun db:collections ()
  (loop for row in (lambdalite:select :schemas)
        for name = (string (getf row :/name))
        for cpos = (position #\: name)
        for realname = (if cpos
                           (find-symbol (subseq name (1+ cpos)) (subseq name 0 cpos))
                           name)
        when realname collect realname))

(defun db:collection-exists-p (collection)
  (not (null (lambdalite:select :schemas (lambdalite:where (eql :/name (ensure-collection collection)))))))

(defun check-field-type (field type)
  (unless (typecase type
            ((eql :id) T)
            ((eql :integer) T)
            ((eql :float) T)
            ((eql :character) T)
            ((eql :text) T)
            (cons (case (first type)
                    (:integer (typep (second type) '(integer 1 8)))
                    (:varchar (typep (second type) '(integer 1))))))
    (error 'db:invalid-field :field field)))

(defun check-field-name (field)
  (unless (typecase field
            (string (valid-name-p field))
            (symbol (valid-name-p (string field))))
    (error 'db:invalid-field :field field)))

(defun db:create (collection structure &key indices (if-exists :ignore))
  (declare (ignore indices))
  (loop for (name type) in structure
        do (check-field-name name)
           (check-field-type name type))
  (when (db:structure collection)
    (ecase if-exists
      (:ignore (return-from db:create NIL))
      (:error (error 'db:collection-already-exists :collection collection :database *db-name*))
      (:supersede (db:drop collection))))
  (lambdalite:insert :schemas `(:/name ,(ensure-collection collection)
                                :/structure ,(loop for (name type) in structure
                                                   collect (list (string name) type))))
  T)

(defun db:structure (collection)
  (getf
   (lambdalite:select1 :schemas (lambdalite:where (eql :/name (ensure-collection collection))))
   :/structure))

(defun db:empty (collection)
  (lambdalite:del (ensure-collection collection) (constantly T)))

(defun db:drop (collection)
  (unless (db:structure collection)
    (error 'db:invalid-collection :collection collection))
  (let ((collection (ensure-collection collection)))
    (lambdalite:del :schemas (lambdalite:where (eql :/name collection)))
    (lambdalite::with-lock
      (remhash collection lambdalite::*db*))))

(defun sort-by-specs (list specs)
  (let ((sortfunc))
    (flet ((sorter (a b dir)
             (unless sortfunc
               (setf sortfunc
                     (etypecase a
                       (integer
                        (case dir
                          (:ASC #'<)
                          (:DESC #'>)))
                       (string
                        (case dir
                          (:ASC #'string<)
                          (:DESC #'string>))))))
             (funcall sortfunc a b)))
      (dolist (spec specs)
        (let ((field (ensure-field (first spec)))
              (order (second spec)))
          (setf list (sort list (lambda (a b)
                                  (sorter (getf a field)
                                          (getf b field)
                                          order))))))
      list)))

(defun db:iterate (collection query function &key fields (skip 0) amount sort accumulate)
  (flet ((row-processor (row)
           (funcall function
                    (let ((table (make-hash-table :test 'equalp)))
                      (if fields
                          (dolist (field fields)
                            (setf (gethash (subseq (string field) 1) table)
                                  (getf row (ensure-field field))))
                          (loop for (field val) on row by #'cddr
                                do (setf (gethash (subseq (string field) 1) table) val)))
                      table))))
    (let ((data (loop for i from 0
                      for row in (sort-by-specs (lambdalite:select (ensure-collection collection) query) sort)
                      while (or (not amount)
                                (< i (+ skip amount)))
                      when (<= skip i)
                      collect row)))
      (if accumulate
          (mapcar #'row-processor data)
          (map NIL #'row-processor data)))))

(defun db:select (collection query &key fields (skip 0) amount sort)
  (db:iterate collection query #'identity
              :fields fields :skip skip :amount amount :sort sort :accumulate T))

(defun db:count (collection query)
  (length (db:select collection query :fields '(:_id))))

(defun db:insert (collection data)
  (let* ((id (make-row-id collection))
         (list (list :/_id id)))
    (etypecase data
      (hash-table
       (maphash (lambda (key val) (setf (getf list (ensure-field key)) val)) data))
      (list
       (loop for (key . val) in data do (setf (getf list (ensure-field key)) val))))
    (lambdalite:insert (ensure-collection collection) list)
    id))

(defun db:remove (collection query &key (skip 0) amount sort)
  (with-table-change (collection rows)
    (let ((i 0))
      (delete-if (lambda (row)
                   (prog1 (and (<= skip i)
                               (or (not amount)
                                   (< i (+ skip amount)))
                               (funcall query row))
                     (incf i)))
                 (sort-by-specs rows sort)))))

(defun db:update (collection query data &key (skip 0) amount sort)
  (let ((setter (etypecase data
                  (hash-table
                   (lambda (row)
                     (maphash (lambda (key val)
                                (setf (getf row (ensure-field key)) val))
                              data)))
                  (list
                   (lambda (row)
                     (loop for (key . val) in data
                           do (setf (getf row (ensure-field key)) val)))))))
    (with-table-change (collection rows)
      (prog1 (setf rows (sort-by-specs rows sort))
        (loop for row in rows
              for i from 0
              while (or (not amount)
                        (< i (+ skip amount)))
              when (and (<= skip i)
                        (funcall query row))
              do (funcall setter row))))
    NIL))

(defmacro db:with-transaction (() &body body)
  `(lambdalite:with-tx
     ,@body))

(defvar *rowvar*)
(defmacro db:query (query-form)
  (if (eql query-form :all)
      `(constantly T)
      (let ((*rowvar* (gensym "ROW")))
        `(lambda (,*rowvar*)
           ,(compile-query query-form)))))

(defun gencomp (a b numcomp stringcomp)
  (etypecase a
    (number (funcall numcomp a b))
    (string (funcall stringcomp a b))))

(defun compile-query (form)
  (etypecase form
    (null (error "NIL not allowed"))
    ((or real string symbol) form)
    (character (string form))
    (list
     (flet ((gencomparator (numcomp stringcomp)
              `(gencomp ,(compile-query (second form))
                        ,(compile-query (third form))
                        (function ,numcomp) (function ,stringcomp))))
       (case (first form)
         (:= `(equal ,(compile-query (second form))
                     ,(compile-query (third form))))
         (:!= (compile-query `(:not (:= ,@(cdr form)))))
         (:> (gencomparator '> 'string>))
         (:< (gencomparator '< 'string<))
         (:<= (gencomparator '< 'string<=))
         (:>= (gencomparator '>= 'string>=))
         (:MATCHES `(cl-ppcre:scan ,(compile-query (third form))
                                   ,(compile-query (second form))))
         (:IN `(find ,(compile-query (second form))
                     (list ,@(mapcar #'compile-query (cddr form)))
                     :test #'equal))
         (:AND `(and ,@(mapcar #'compile-query (cdr form))))
         (:OR `(or ,@(mapcar #'compile-query (cdr form))))
         (:NOT `(not ,(compile-query (second form))))
         (:FIELD `(getf ,*rowvar* ,(ensure-field (second form))))
         (QUOTE (compile-query `(:FIELD ,(second form))))
         (T form))))))

(define-trigger server-start ()
  (defaulted-config "radiance.db" :connections "radiance")
  (defaulted-config "test.db" :connections "test")
  (db:connect (defaulted-config "radiance" :default)))

(define-trigger server-stop ()
  (db:disconnect))
