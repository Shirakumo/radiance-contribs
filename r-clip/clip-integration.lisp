#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:r-clip
  (:use #:cl #:radiance)
  (:export #:process #:lquery-wrapper))
(in-package #:r-clip)

(defpackage #:radiance-clip
  (:use #:cl #:radiance #:clip)
  (:shadow #:or*))

(defun radiance-clip::or* (&rest vals)
  (loop for val in vals
        thereis (if (stringp val)
                    (unless (string= val "") val)
                    val)))

(defmethod clip:clip ((object standard-object) field)
  (field object field))

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

(define-page-option lquery (page uri body template)
  (if template
      `((setf (content-type *response*) "application/xhtml+xml; charset=utf-8")
        ,@(transform-body body template))
      body))

(define-implement-hook admin
  (admin:define-panel-option lquery (name category body template)
    (transform-body body template)))

(define-implement-hook profile
  (profile:define-panel-option lquery (name body template)
    (transform-body body template)))

(defun process-pattern (value node attribute)
  (when (< 0 (length value))
    (let ((args ()))
      (flet ((parse (value)
               (cond ((char= (aref value 0) #\()
                      (let ((read (read-from-string value)))
                        (setf args (mapcar #'clip:resolve-value (rest read)))
                        (first read)))
                     (T
                      (parse-pattern value)))))
        (setf (plump:attribute node attribute)
              (uri-to-url (apply #'resolve (parse value) args) :representation :external))))))

(macrolet ((define-pattern-attribute (name)
             (let ((symb (intern (concatenate 'string "@" (string name)))))
               `(clip:define-attribute-processor ,symb (node value)
                  (plump:remove-attribute node ,(string-downcase symb))
                  (process-pattern value node ,(string-downcase name))))))
  (define-pattern-attribute href)
  (define-pattern-attribute src)
  (define-pattern-attribute link)
  (define-pattern-attribute action))


(defun date-machine (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-human (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2)))))

(defun date-fancy (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (local-time:format-timestring
     NIL stamp :format '(:long-weekday ", " :ordinal-day " of " :long-month " " :year ", " :hour ":" (:min 2) ":" (:sec 2) " UTC"))))

(lquery:define-lquery-function format-time (node time)
  (let ((stamp (etypecase time
                 (local-time:timestamp time)
                 (fixnum (local-time:universal-to-timestamp time))
                 (string (local-time:parse-timestring time)))))
    (setf (plump:attribute node "datetime")
          (date-machine stamp))
    (setf (plump:attribute node "title")
          (date-fancy stamp))
    (setf (plump:children node) (plump:make-child-array))
    (plump:make-text-node node (date-human stamp)))
  node)
