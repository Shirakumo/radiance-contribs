#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:r-orm)

(defclass collection-class (standard-class)
  ((fields :initarg :fields :initform NIL :accessor fields)
   (collection :initarg :collection :initform T :accessor collection))
  (:documentation ""))

(defmethod c2mop:validate-superclass ((class collection-class) (superclass t)) NIL)
(defmethod c2mop:validate-superclass ((class standard-class) (superclass collection-class)) NIL)
(defmethod c2mop:validate-superclass ((class collection-class) (superclass standard-class)) T)
(defmethod c2mop:validate-superclass ((class collection-class) (superclass collection-class)) T)

(defclass collection-class-slot ()
  ((field-type :initarg :field-type :initform NIL :reader field-type)
   (field-name :initarg :field-name :initform NIL :reader field-name)
   (field-indexed :initarg :indexed :initform NIL :reader field-indexed)))

(defmethod initialize-instance :after ((slot collection-class-slot) &key)
  (when (and (field-type slot)
             (not (field-name slot)))
    (setf (slot-value slot 'field-name)
          (string-downcase (c2mop:slot-definition-name slot)))))

(defmethod print-object ((slot collection-class-slot) stream)
  (print-unreadable-object (slot stream :type T)
    (format stream "~a ~s~:[~; indexed~]"
            (field-name slot) (field-type slot) (field-indexed slot))))

(defclass collection-class-direct-slot (collection-class-slot c2mop:standard-direct-slot-definition) ())
(defclass collection-class-effective-slot (collection-class-slot c2mop:standard-effective-slot-definition) ())

(defmethod c2mop:direct-slot-definition-class ((class collection-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'collection-class-direct-slot))

(defmethod c2mop:effective-slot-definition-class ((class collection-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'collection-class-effective-slot))

(defun copy-collection-slot-slots (from to)
  (loop for slot in (c2mop:class-direct-slots (find-class 'collection-class-slot))
        for slot-name = (c2mop:slot-definition-name slot)
        do (setf (slot-value to slot-name)
                 (slot-value from slot-name))))

(defmethod c2mop:compute-effective-slot-definition ((class collection-class) name direct-slots)
  (let ((effective-slot (call-next-method)))
    (loop for direct-slot in direct-slots
          do (when (and (typep direct-slot 'collection-class-direct-slot)
                        (eql (c2mop:slot-definition-name direct-slot)
                             (c2mop:slot-definition-name effective-slot)))
               (copy-collection-slot-slots direct-slot effective-slot)
               (return)))
    effective-slot))

(defun collection-from-symbol (symbol)
  (format NIL "~a-~a"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defun slot-field-definition (slot)
  (assert (not (null (field-type slot)))
          () "~s is not a field slot." slot)
  (list* (c2mop:slot-definition-name slot)
         (field-name slot)
         (field-type slot)
         (when (field-indexed slot) (list :indexed))))

(defun compute-fields-from-slots (slots)
  (loop for slot in slots
        when (and (typep slot 'collection-class-effective-slot)
                  (field-type slot))
        collect (slot-field-definition slot)))

(defun check-field-validity (field)
  (handler-case
      (destructuring-bind (slot field type &optional indexed) field
        (declare (ignore indexed))
        (check-type slot symbol)
        (check-type field string)
        (check-type type (or keyword list)))
    (error (err)
      (error "FIELD definition ~s is malformed! ~a" field err))))

(defmacro do-fields ((collection slot &optional name type indexed) &body body)
  (let ((field (gensym "FIELD"))
        (name (or name (gensym "NAME")))
        (type (or type (gensym "TYPE")))
        (indexed (or indexed (gensym "INDEXED"))))
    `(dolist (,field (fields ,collection))
       (destructuring-bind (,slot ,name ,type &optional ,indexed) ,field
         (declare (ignorable ,slot ,name ,type ,indexed))
         ,@body))))

(defmacro do-collect-fields ((collection slot &optional name type indexed) &body body)
  (let ((field (gensym "FIELD"))
        (name (or name (gensym "NAME")))
        (type (or type (gensym "TYPE")))
        (indexed (or indexed (gensym "INDEXED")))
        (list (gensym "LIST")))
    `(let ((,list ()))
       (dolist (,field (fields ,collection) (nreverse ,list))
         (push
          (destructuring-bind (,slot ,name ,type &optional ,indexed) ,field
            (declare (ignorable ,slot ,name ,type ,indexed))
            ,@body)
          ,list)))))

(defun ensure-class-collection (class)
  (let ((collection (collection class))
        (fields (delete "_id"
                        (do-collect-fields (class slot name type)
                          (list name type))
                        :test #'string-equal :key #'first))
        (indices (loop for field in (fields class)
                       when (fourth field)
                       collect (second field))))
    (cond ((and (db:implementation)
                (db:connected-p))
           (if (db:collection-exists-p collection)
               (warn "Collection ~s already exists, not adapting schema on database." collection)
               (db:create collection fields :indices indices)))
          (T (funcall
              (compile NIL `(lambda ()
                              (hooks:define-trigger (db:connected ',(class-name class)) ()
                                (db:create ,collection ',fields :indices ',indices)))))))))

(defun initialize-collection-class (class next-method &rest args &key fields &allow-other-keys)
  ;; Do things so we have slots
  (apply next-method class args)
  (c2mop:finalize-inheritance class)
  
  (unless fields
    (setf (fields class) (compute-fields-from-slots (c2mop:class-slots class))))
  
  (mapc #'check-field-validity (fields class))

  (when (listp (collection class))
    (setf (collection class) (first (collection class))))

  (when (eql (collection class) T)
    (setf (collection class) (collection-from-symbol (class-name class))))

  (when (collection class)
    (assert (stringp (collection class))
            () "COLLECTION of class must be a string.")
    (ensure-class-collection class))
  class)

(defmethod initialize-instance :around ((class collection-class) &rest args)
  (apply #'initialize-collection-class class #'call-next-method args))

(defmethod reinitialize-instance :around ((class collection-class) &rest args)
  (apply #'initialize-collection-class class #'call-next-method args))

(defclass collection ()
  ((_id :initarg :_id :initform NIL :accessor _id :field-type :ID))
  (:metaclass collection-class)
  (:collection))

(defmethod fields ((collection collection))
  (fields (class-of collection)))

(defmethod collection ((collection collection))
  (collection (class-of collection)))

(defun table-data (collection)
  (let ((fields ()))
    (do-fields (collection slot name)
      (unless (eql slot '_id)
        (push (cons name (slot-value collection slot)) fields)))
    (nreverse fields)))

(defvar +no-value+ (gensym "no-value"))
(defun (setf table-data) (table collection)
  (do-fields (collection slot name)
    (let ((value (gethash name table +no-value+)))
      (unless (eq value +no-value+)
        (setf (slot-value collection slot) value))))
  table)

(defun fetch (collection query &key (skip 0) amount sort)
  (let ((class (find-class collection)))
    (db:iterate (collection class)
                query
                #'(lambda (table)
                    (let ((collection (make-instance collection)))
                      (setf (table-data collection) table)
                      collection))
                :skip skip :amount amount :sort sort :accumulate T)))

(defun fetch-one (collection query &key (skip 0) sort)
  (first (fetch collection query :skip skip :sort sort :amount 1)))

(defun insert (collection)
  (assert (null (_id collection))
          () "Cannot INSERT an inserted collection instance.")
  (setf (_id collection)
        (db:insert (collection collection)
                   (table-data collection)))
  collection)

(defun save (collection)
  (assert (not (null (_id collection)))
          () "Cannot SAVE an un-inserted collection instance.")
  (db:update (collection collection)
             (db:query (:= '_id (_id collection)))
             (table-data collection)
             :amount 1)
  collection)

(defun purge (collection)
  (assert (not (null (_id collection)))
          () "Cannot DELETE an un-inserted collection instance.")
  (db:remove (collection collection)
             (db:query (:= '_id (_id collection)))
             :amount 1)
  (setf (_id collection) NIL)
  collection)

(defmacro define-collection (name direct-superclasses direct-slots &rest options)
  (when (loop for super in direct-superclasses
              never (c2mop:subclassp (find-class super) (find-class 'collection)))
    (push 'collection direct-superclasses))
  `(defclass ,name ,direct-superclasses ,direct-slots ,@options (:metaclass collection-class)))
