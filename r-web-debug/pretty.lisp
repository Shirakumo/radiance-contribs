#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:web-debug
  (:use #:cl #:radiance))
(in-package #:web-debug)

(defun slots (object)
  (when (ignore-errors (class-of object)) ;; The CLHS doesn't specify what happens if there is no class.
    (loop for slot in (c2mop:class-slots (class-of object))
          collect (list (c2mop:slot-definition-name slot)
                        (slot-value object (c2mop:slot-definition-name slot))))))

(defun radiance:present-error-page (condition)
  (setf (return-code *response*) 500)
  (setf (content-type *response*) "application/xthml+xml")
  (plump:serialize
   (r-clip:process
    (plump:parse (template "error.ctml"))
    :condition condition
    :stack (dissect::stack)
    :restarts (dissect::restarts)
    :objects (remove-if #'null (list condition *request* *response*)))
   T))
