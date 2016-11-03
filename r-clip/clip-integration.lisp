#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:r-clip)

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
             ((eql T) lquery:*lquery-master-document*)
             (pathname (plump:parse target))
             (string (plump:parse target))
             (plump:node target))
           fields)))

(defmacro lquery-wrapper ((template &optional (content-type "application/xhtml+xml; charset=utf-8")) &body body)
  `(let ((lquery:*lquery-master-document* (lquery:load-page (template ,template))))
     (setf (content-type *response*) ,content-type)
     (handler-bind ((plump:invalid-xml-character #'abort))
       ,@body
       (lquery:$ (serialize) (node)))))

(defun transform-body (body template)
  (if template
      `((let* ((lquery:*lquery-master-document*
                 (lquery:load-page ,template)))
          (handler-bind ((plump:invalid-xml-character #'abort))
            ,@body
            (lquery:$ (serialize) (node)))))
      body))

(define-option radiance:page lquery (page body uri &optional template)
  (if template
      `((setf (content-type *response*) "application/xhtml+xml; charset=utf-8")
        ,@(transform-body body template))
      body))

(define-option admin:panel lquery (name body category &optional template)
  (transform-body body template))

(define-option profile:panel lquery (name body &optional template)
  (transform-body body template))

(defun process-pattern (value node attribute)
  (when (< 0 (length value))
    (let ((args (parse-pattern value)))
      (setf (plump:attribute node attribute)
            (uri-to-url (apply #'resolve args) :representation :external)))))

(macrolet ((define-pattern-attribute (name)
             (let ((symb (intern (concatenate 'string "@" (string name)))))
               `(clip:define-attribute-processor ,symb (node value)
                  (plump:remove-attribute node ,(string-downcase symb))
                  (process-pattern value node ,(string-downcase name))))))
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
