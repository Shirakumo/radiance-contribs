#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:r-clip)

(defvar *document*)

(defun radiance-clip::or* (&rest vals)
  (loop for val in vals
        thereis (if (stringp val)
                    (unless (string= val "") val)
                    val)))

(defmethod clip:clip ((object dm:data-model) field)
  (dm:field object field))

(defun process (target &rest fields)
  (let ((*package* (find-package "RADIANCE-CLIP")))
    (apply #'clip:process
           (etypecase target 
             ((eql T) *document*)
             (pathname (plump:parse target))
             (string (plump:parse target))
             (plump:node target))
           fields)))

(defmacro with-clip-processing ((template &optional (content-type "application/xhtml+xml; charset=utf-8")) &body body)
  (let ((result (gensym "RESULT")))
    `(let ((*document* (plump:parse ,(if (stringp template)
                                         `(@template ,template)
                                         template))))
       (flet ((,result ()
                ,@body))
         (let ((,result (,result)))
           (typecase ,result
             (plump:node
              (setf (content-type *response*) ,content-type)
              (handler-bind ((plump:invalid-xml-character #'abort)
                             (plump:discouraged-xml-character #'muffle-warning))
                (plump:serialize ,result NIL)))
             (T ,result)))))))

(defmacro switch-template (template)
  `(setf *document* (lquery:load-page (@template ,template))))

(defun transform-body (body template)
  (if template
      `((with-clip-processing (,template)
          ,@body))
      body))

(define-option radiance:page :clip (name body uri &optional template)
  (declare (ignore name uri))
  (transform-body body template))

(define-option admin:panel :clip (name body category &optional template)
  (declare (ignore name category))
  (transform-body body template))

(define-option profile:panel :clip (name body &optional template)
  (declare (ignore name))
  (transform-body body template))

(macrolet ((define-pattern-attribute (name)
             (let ((symb (intern (concatenate 'string "@" (string name)))))
               `(clip:define-attribute-processor ,symb (node value)
                  (plump:remove-attribute node ,(string-downcase symb))
                  (when (< 0 (length value))
                    (setf (plump:attribute node ,(string-downcase name))
                          (parse-uri* value)))))))
  (define-pattern-attribute href)
  (define-pattern-attribute src)
  (define-pattern-attribute link)
  (define-pattern-attribute action)
  (define-pattern-attribute formaction))

(lquery:define-lquery-function time (node time)
  (let ((stamp (etypecase time
                 (local-time:timestamp time)
                 (fixnum (local-time:universal-to-timestamp time))
                 (string (local-time:parse-timestring time)))))
    (setf (plump:attribute node "datetime")
          (format-machine-date stamp))
    (setf (plump:attribute node "title")
          (format-fancy-date stamp))
    (setf (plump:children node) (plump:make-child-array))
    (plump:make-text-node node (format-human-date stamp)))
  node)
