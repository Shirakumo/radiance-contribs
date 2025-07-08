(in-package #:i-postmodern)

(defun exec-query (query vars &optional (row-reader 'cl-postgres:ignore-row-reader))
  (with-connection
    (l:trace :database "QUERY: ~s ~s" query vars)
    (cl-postgres:prepare-query *current-con* "" query)
    (cl-postgres:exec-prepared *current-con* "" vars row-reader)))

(defun %field (s a c p)
  (declare (ignore c p))
  (cond ((not (consp a))
         (format s "~(~s~)" (string a)))
        ((second a)
         (format s "~(~@[~a.~]~s~)"
                 (first a) (string (second a))))
        (T
         (format s "~(~s~)" (string (first a))))))

(defun %sort-clause (s a c p)
  (declare (ignore c p))
  (let* ((field (pop a))
         (order (pop a))
         (table (if a (shiftf field order (pop a)))))
    (ecase order (:DESC) (:ASC))
    (format s "~(~@[~a.~]\"~a\"~) ~a" table field order)))

(defun make-query (base where skip amount sort)
  (assert (or (null amount) (integerp amount)))
  (assert (or (null skip) (integerp skip)))
  (assert (listp sort))
  (with-query (where where vars)
    (cons (format NIL "~a ~a~@[ ORDER BY ~{~/i-postmodern::%sort-clause/~^, ~}~]~@[ LIMIT ~d~]~:[~; OFFSET ~d~]"
                  base where sort amount (and skip (/= skip 0)) skip)
          vars)))

(defvar *vars*)
(defmacro db:query (query-form)
  (let ((*vars* ()))
    (if (eql query-form :ALL)
        `(cons "" ())
        `(cons ,(format NIL "WHERE ~a" (compile-form query-form))
               (list ,@(nreverse *vars*))))))

(defun compile-form (form)
  (etypecase form
    (null "FALSE")
    ((eql T) "TRUE")
    (symbol
     (push form *vars*)
     (format NIL "$~a" (length *vars*)))
    (real form)
    (character form)
    (string
     (format NIL "'~a'" form))
    (list
     (case (first form)
       (:= (format NIL "(~a) = (~a)" (compile-form (second form)) (compile-form (third form))))
       ((:/= :!=) (format NIL "(~a) != (~a)" (compile-form (second form)) (compile-form (third form))))
       (:> (format NIL "(~a) > (~a)" (compile-form (second form)) (compile-form (third form))))
       (:< (format NIL "(~a) < (~a)" (compile-form (second form)) (compile-form (third form))))
       (:<= (format NIL "(~a) <= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:>= (format NIL "(~a) >= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:MATCHES (format NIL "(~a) ~~ (~a)" (compile-form (second form)) (compile-form (third form))))
       (:MATCHES* (format NIL "(~a) ~~* (~a)" (compile-form (second form)) (compile-form (third form))))
       (:IN (format NIL "(~a) IN (~{~a~^, ~})" (compile-form (second form)) (mapcar #'compile-form (cddr form))))
       (:ANY (format NIL "(~a) ~a ANY(~a)" (compile-form (second form)) (or (fourth form) :=) (compile-form (third form))))
       (:AND (format NIL "~{(~a)~^ AND ~}" (mapcar #'compile-form (rest form))))
       (:OR (format NIL "~{(~a)~^ OR ~}" (mapcar #'compile-form (rest form))))
       (:NOT (format NIL "NOT (~a)" (compile-form (second form))))
       (:FIELD (if (third form)
                   (format NIL "~(~@[~a~].\"~a\"~)" (second form) (third form))
                   (format NIL "~(\"~a\"~)" (second form))))
       (:NULL (format NIL "(~a) IS NULL" (compile-form (second form))))
       (QUOTE (format NIL "~(\"~a\"~)" (second form)))
       (T (push form *vars*)
        (format NIL "$~a" (length *vars*)))))))

(defstruct (join (:constructor make-join (string)))
  (string NIL :type string :read-only T))

(defmethod make-load-form ((join join) &optional environment)
  (declare (ignore environment))
  `(make-join ,(join-string join)))

(defmacro rdb:join (&whole operand (left-collection left-field) (right-collection right-field) &optional (type :inner))
  (declare (ignore left-collection left-field right-collection right-field type))
  (make-join (compile-join-operand (rest operand))))

(defun compile-join-operand (operand)
  (destructuring-bind ((left-operand left-field) (right-operand right-field) &optional (type :inner))
      operand
    (flet ((operand-string (operand)
             (etypecase operand
               (symbol (ensure-collection-name operand))
               (cons (compile-join-operand operand)))))
      (format NIL "(~a AS a ~a JOIN ~a AS b ON ~(A.~s = B.~s~))"
              (operand-string left-operand)
              (ecase type
                (:inner "INNER")
                (:left "LEFT")
                (:right "RIGHT")
                (:outer "FULL"))
              (operand-string right-operand)
              (symbol-name left-field)
              (symbol-name right-field)))))
