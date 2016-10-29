#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-errors
  (:use #:cl #:radiance))
(in-package #:simple-errors)

(defun process (template &rest args)
  (plump:serialize
   (apply #'r-clip:process
          (plump:parse (template template))
          args)
   NIL))

(defun radiance:render-error-page (condition)
  (etypecase condition
    (request-empty
     (setf (return-code *response*) 204)
     "")
    (request-not-found
     (setf (return-code *response*) 404)
     (process "not-found.ctml"))
    (request-denied
     (setf (return-code *response*) 403)
     (cond ((and (auth:implementation) (not (auth:current)))
            (process "login-required.ctml"))
           (T
            (process "access-denied.ctml"))))
    (error
     (setf (return-code *response*) 500)
     (process "internal-error.ctml"))))
