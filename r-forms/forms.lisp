#|
 This file is a part of Radiance
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.forms)

(defvar *parent*)

(defun attrs (&rest attributes)
  (let ((map (plump-dom:make-attribute-map)))
    (loop for (key val) on attributes by #'cddr
          do (setf (gethash (string-downcase key) map)
                   (princ-to-string val)))
    map))

(defun ensure-uri-string (uri)
  (etypecase uri
    (string uri)
    (radiance:uri
     (radiance:uri-to-url uri :representation :external))))

(defmacro text (&body texts)
  `(plump-dom:make-text-node *parent* (concatenate
                                       'string
                                       ,@(mapcar (lambda (a) `(princ-to-string ,a)) texts))))

(defmacro root (&body contents)
  `(let ((*parent* (plump-dom:make-root)))
     ,@contents
     *parent*))

(defmacro element (tag attributes &body contents)
  `(let ((*parent* (plump-dom:make-element *parent* ,tag :attributes (attrs ,@attributes))))
     ,@contents
     *parent*))

(defmacro span (attributes &body contents)
  `(element "span" ,attributes
     ,@contents))

(defmacro div (attributes &body contents)
  `(element "div" ,attributes
     ,@contents))

(defmacro label (name &optional value)
  `(span () (element "label" () (text ,name ": "))
     (text ,value " ")))

(defmacro form (action attributes &body contents)
  `(element "form" (:action (ensure-uri-string ,action) ,@attributes)
     ,@contents))

(defmacro input (type attributes)
  `(element "input" (:type ,type ,@attributes)))

(defmacro submit (&optional attributes)
  `(input "submit" (:name "action" :value "Submit" ,@attributes)))

(defmacro option (value attributes &body contents)
  `(element "option" (:value ,value ,@attributes)
     ,@contents))

(defmacro select (name attributes &body contents)
  `(element "select" (:name ,name ,@attributes)
     ,@contents))

(defmacro checkbox (name value attributes &body contents)
  `(div ()
     (input "checkbox" (:name ,name :value ,value ,@attributes))
     ,@contents))

(defmacro radio (name value attributes &body contents)
  `(div ()
     (input "radio" (:name ,name :value ,value ,@attributes))
     ,@contents))

(defmacro fieldset (attributes &body contents)
  `(element "fieldset" ,attributes
     ,@contents))

(defmacro define-endpoint (name args &body body)
  `(defun ,name ,args
     (plump-dom:serialize
      (root
        ,@body)
      NIL)))

(defmacro do-options ((id option) options-form &body body)
  (let ((options (gensym "OPTIONS")))
    `(let ((,options ,options-form))
       (dolist (,option ,options)
         (etypecase ,option
           (dm:data-model
            (let ((,id (dm:id ,option)))
              ,@body))
           (list
            (let ((,id (first ,option))
                  (,option (rest ,option)))
              ,@body)))))))

(defmacro do-fields ((name value) model-form &body body)
  (let ((model (gensym "MODEL")))
    `(let ((,model ,model-form))
       (etypecase ,model
         (dm:data-model
          (loop for ,name in (dm:fields ,model)
                for ,value = (dm:field ,model ,name)
                do (progn ,@body)))
         (list
          (etypecase (first ,model)
            (keyword
             (loop for (,name ,value) on ,model by #'cddr
                   do (progn ,@body)))
            (cons
             (loop for (,name ,value) in ,model
                   do (progn ,@body)))))))))

(define-endpoint choose (action name options)
  (form action ()
    (fieldset ()
      (do-options (id option) options
        (radio name id ()
          (do-fields (field value) option
            (label field value)))))
    (submit)))

(define-endpoint pick (action name options)
  (form action ()
    (fieldset ()
      (do-options (id option) options
        (checkbox name id ()
          (do-fields (field value) option
            (label field value)))))
    (submit)))
