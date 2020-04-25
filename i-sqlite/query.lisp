#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(defmacro with-query ((query-form &optional (where 'where) (vars 'vars)) &body body)
  (let ((res (gensym "RESULT")))
    `(let* ((,res ,query-form)
            (,where (car ,res))
            (,vars (cdr ,res)))
       ,@body)))

(defun exec-query (query vars &optional (row-reader #'(lambda (statement) (declare (ignore statement)))))
  (l:trace :database "QUERY: ~s ~s" query vars)
  (let ((result))
    (with-simple-restart (skip "Skip the query and hope things work out.")
      (loop until (with-simple-restart (retry "Retry the query.")
                    (let ((statement (sqlite:prepare-statement *current-con* query)))
                      (loop for i from 1
                            for var in vars
                            do (sqlite:bind-parameter statement i var))
                      (unwind-protect
                           (when (sqlite:step-statement statement)
                             (setf result (funcall row-reader statement)))
                        (sqlite:finalize-statement statement)))
                    T)))
    result))

(defun %sort-clause (s a c p)
  (declare (ignore c p))
  (let ((field (first a))
        (order (second a)))
    (ecase order (:DESC) (:ASC))
    (format s "~s ~a" (string-downcase field) order)))

(defun make-query (base where skip amount sort)
  (check-type amount (or null (integer 1)))
  (check-type skip (or null (integer 0)))
  (check-type sort list)
  (with-query (where where vars)
    (cons (format NIL "~a ~a~@[ ORDER BY ~{~/i-sqlite::%sort-clause/~^, ~}~]~@[ LIMIT ~d~]~:[~; OFFSET ~d~]"
                  base where sort (or amount (when skip -1)) (and skip (/= skip 0)) skip)
          vars)))

(defvar *vars*)
(defun compile-form (form)
  (etypecase form
    (null "FALSE")
    ((eql T) "TRUE")
    (symbol
     (push form *vars*)
     "?")
    (real form)
    (character form)
    (string
     (format NIL "'~a'" form))
    (list
     (case (first form)
       (:= (format NIL "(~a) = (~a)" (compile-form (second form)) (compile-form (third form))))
       (:!= (format NIL "(~a) != (~a)" (compile-form (second form)) (compile-form (third form))))
       (:> (format NIL "(~a) > (~a)" (compile-form (second form)) (compile-form (third form))))
       (:< (format NIL "(~a) < (~a)" (compile-form (second form)) (compile-form (third form))))
       (:<= (format NIL "(~a) <= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:>= (format NIL "(~a) >= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:MATCHES (format NIL "(~a) REGEXP (~a)" (compile-form (second form)) (compile-form (third form))))
       (:IN (format NIL "(~a) IN (~{~a~^, ~})" (compile-form (second form)) (mapcar #'compile-form (cddr form))))
       (:AND (format NIL "~{(~a)~^ AND ~}" (mapcar #'compile-form (rest form))))
       (:OR (format NIL "~{(~a)~^ OR ~}" (mapcar #'compile-form (rest form))))
       (:NOT (format NIL "NOT (~a)" (compile-form (second form))))
       (:FIELD (format NIL "\"~a\"" (string-downcase (second form))))
       (:NULL (format NIL "(~a) IS NULL" (compile-form (second form))))
       (QUOTE (format NIL "\"~a\"" (string-downcase (second form))))
       (T (push form *vars*)
	"?")))))

(defmacro db:query (query-form)
  (let ((*vars* ()))
    (if (eql query-form :ALL)
        `(cons "" ())
        `(cons ,(format NIL "WHERE ~a" (compile-form query-form))
               (list ,@(nreverse *vars*))))))

(defvar *join-table-names*
  (loop for char across "abcdefghijklmnopqrstuvwxyz"
        collect (string char)))

(defstruct (join (:constructor make-join (string collections)))
  (string NIL :type string :read-only T)
  (collections NIL :type list :read-only T))

(defmethod make-load-form ((join join) &optional environment)
  (declare (ignore environment))
  `(make-join ,(join-string join) ',(join-collections join)))

(defmacro rdb:join (&whole operand (left-collection left-field) (right-collection right-field) &optional (type :inner))
  (declare (ignore left-collection left-field right-collection right-field type))
  (let ((*join-table-names* *join-table-names*))
    (make-join (compile-join-operand (rest operand))
               (compile-join-collections (rest operand)))))

(defun compile-join-collections (operand)
  (destructuring-bind ((left-operand _l) (right-operand _r) &optional _t) operand
    (declare (ignore _l _r _t))
    (flet ((operand (operand)
             (etypecase operand
               (symbol (list (coerce-collection-name operand)))
               (cons (compile-join-collections operand)))))
      (append (operand left-operand)
              (operand right-operand)))))

(defun compile-join-operand (operand)
  (destructuring-bind ((left-operand left-field) (right-operand right-field) &optional (type :inner))
      operand
    (let ((lname (pop *join-table-names*))
          (rname (pop *join-table-names*)))
      (flet ((operand-string (operand)
               (etypecase operand
                 (symbol (ensure-collection-name operand))
                 (cons (compile-join-operand operand)))))
        (format NIL "(~a AS ~a ~a JOIN ~a AS ~a ON ~(~a.~s = ~a.~s~))"
                (operand-string left-operand) lname
                (ecase type
                  (:inner "INNER")
                  (:left "LEFT")
                  (:right "RIGHT")
                  (:outer "FULL"))
                (operand-string right-operand) rname
                lname (symbol-name left-field)
                rname (symbol-name right-field))))))
