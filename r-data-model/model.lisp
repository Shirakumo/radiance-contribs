#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.data-model)

(define-condition not-inserted-yet (error radiance:radiance-condition)
  ((model :initarg :model :initform (error "MODEL required."))))

(defclass data-model ()
  ((collection :initform (error "COLLECTION required.") :initarg :collection :accessor collection)
   (field-table :initform (make-hash-table :test 'equal) :initarg :field-table :accessor field-table)
   (field-labels :initform () :initarg :field-labels :accessor field-labels)
   (inserted :initform NIL :initarg :inserted :accessor inserted)))

(defmethod print-object ((model data-model) stream)
  (print-unreadable-object (model stream :type NIL)
    (format stream "DATA-MODEL ~a:~a~@[ HULL~]" (collection model) (id model) (hull-p model))))

(defmethod describe-object ((model data-model) stream)
  (format stream "~a~%  [~s]~%~%Represents record ~a on ~a~%Holds the following fields:"
          model (type-of model) (id model) (collection model))
  (loop for k being the hash-keys of (field-table model) using (hash-value v)
        do (format stream "~%  ~30a: ~s" k v)))

(defun id (data-model)
  (gethash "_id" (field-table data-model)))

(defun fields (data-model)
  (or (field-labels data-model)
      (setf (field-labels data-model)
            (mapcar #'first (db:structure (collection data-model))))))

(defun field (data-model field)
  (gethash (string-downcase field) (field-table data-model)))

(define-compiler-macro field (&whole whole &environment env data-model field)
  (if (constantp field env)
      `(gethash (load-time-value (string-downcase ,field)) (field-table ,data-model))
      whole))

(defun (setf field) (value data-model field)
  (setf (gethash (string-downcase field) (field-table data-model)) value))

(define-compiler-macro (setf field) (&whole whole &environment env value data-model field)
  (if (constantp field env)
      `(setf (gethash (load-time-value (string-downcase ,field)) (field-table ,data-model)) ,value)
      whole))

(defun get (collection query &key (skip 0) amount sort unique (hull collection))
  (db:iterate collection query #'(lambda (ta) (make-instance 'data-model :collection hull :field-table ta :inserted T))
              :skip skip :amount amount :sort sort :unique unique :accumulate T))

(defun get-one (collection query &key (skip 0) sort (hull collection))
  (db:iterate collection query #'(lambda (ta) (return-from get-one
                                                (make-instance 'data-model :collection hull :field-table ta :inserted T)))
              :skip skip :amount 1 :sort sort))

(defun hull (collection) ;; speed up test with extra interface func.
  (unless (db:collection-exists-p collection)
    (error 'db:invalid-collection :collection collection :message "Cannot create hull."))
  (make-instance 'data-model :collection collection))

(defun hull-p (data-model)
  (not (inserted data-model)))

(defun save (data-model)
  (unless (inserted data-model)
    (error 'not-inserted-yet :model data-model))
  (db:update (collection data-model) (db:query (:= '_id (id data-model))) (field-table data-model))
  data-model)

(defun delete (data-model)
  (unless (inserted data-model)
    (error 'not-inserted-yet :model data-model))
  (db:remove (collection data-model) (db:query (:= '_id (id data-model))))
  (setf (inserted data-model) NIL)
  data-model)

(defun copy-hash-table (table)
  (let ((n (make-hash-table :test 'equalp)))
    (maphash #'(lambda (k v) (setf (gethash k n) v)) table)
    n))

(defun copy-model (model)
  (make-instance 'data-model
                 :inserted (inserted model)
                 :field-table (copy-hash-table (field-table model))
                 :collection (collection model)))

(defun insert (data-model &key clone)
  (let ((data-model (if clone (copy-model data-model) data-model)))
    (remhash "_id" (field-table data-model))
    (setf (field data-model "_id")
          (db:insert (collection data-model)
                     (field-table data-model)))
    (setf (inserted data-model) T)
    data-model))

(defun ensure-query-form (form)
  (cond ((eql form :all)
         `(db:query ,form))
        ((not (listp form)) form)
        ((eql (first form) 'db:query)
         form)
        ((keywordp (first form))
         `(db:query ,form))
        (T form)))

(defmacro with-model-fields (object fields &body body)
  (let ((model (gensym "MODEL")))
    `(let ((,model ,object))
       (declare (ignorable ,model))
       (symbol-macrolet
           ,(mapcar (lambda (field)
                      (destructuring-bind (name &optional (field name)) (radiance::enlist field)
                        `(,name (field ,model ,(string-downcase field))))) fields)
         ,@body))))

(defmacro with-model (modelvar (collection query &rest fields) &body body)
  `(let ((,modelvar ,(if query
                         `(get-one ,collection ,(ensure-query-form query))
                         `(hull ,collection))))
     (declare (ignorable ,modelvar))
     ,@(if fields
           `((with-model-fields ,modelvar ,fields
               ,@body))
           body)))

(defmacro with-model-save (modelvar (collection queryform) fields &body body)
  `(with-model ,modelvar (,collection ,queryform ,@fields)
     (prog1
         (progn
           ,@body)
       ,(if queryform
            `(save ,modelvar)
            `(insert ,modelvar)))))

(defmacro do-models (modelvar (collection query &rest fields) &body body)
  `(dolist (,modelvar (get ,collection ,(ensure-query-form query)))
     (with-model-fields ,modelvar ,fields
       ,@body)))

(defmethod radiance:api-serialize ((model data-model))
  (field-table model))
