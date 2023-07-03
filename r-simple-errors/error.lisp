(in-package #:modularize-user)
(define-module simple-errors
  (:use #:cl #:radiance))
(in-package #:simple-errors)

(defun process (template condition)
  (plump:serialize
   (r-clip:process
    (plump:parse (@template template))
    :condition condition
    :reason (or (when (typep condition 'radiance-condition)
                  (format NIL "This might be because: ~a" (message condition)))
                "Unfortunately, no detailed reason for this problem is available."))
   NIL))

(defun radiance:render-error-page (condition)
  (etypecase condition
    (request-empty
     (setf (return-code *response*) 204)
     "")
    (request-not-found
     (setf (return-code *response*) 404)
     (process "not-found.ctml" condition))
    (request-denied
     (setf (return-code *response*) 403)
     (cond ((and (auth:implementation) (not (auth:current)))
            (process "login-required.ctml" condition))
           (T
            (process "access-denied.ctml" condition))))
    (error
     (setf (return-code *response*) 500)
     (process "internal-error.ctml" condition))))
