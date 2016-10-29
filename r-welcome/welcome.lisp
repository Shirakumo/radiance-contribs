#|
 This file is a part of TyNETv5/Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:welcome
    (:use #:cl #:radiance))
(in-package #:welcome)

(defun format-relative-time (seconds)
  (if (= seconds 0)
      (format NIL "0 seconds")
      (let ((seconds   (mod (floor (/ seconds 1)) 60))
            (minutes   (mod (floor (/ seconds 60)) 60))
            (hours     (mod (floor (/ seconds 60 60)) 24))
            (days      (mod (floor (/ seconds 60 60 24)) 7))
            ;; We approximate by saying each month has four weeks
            (weeks     (mod (floor (/ seconds 60 60 24 7)) 4))
            (months    (mod (floor (/ seconds 60 60 24 7 4)) 12))
            ;; More accurate through seconds in a year
            (years     (mod (floor (/ seconds 31557600)) 10))
            (decades   (mod (floor (/ seconds 31557600 10)) 10))
            (centuries (mod (floor (/ seconds 31557600 10 10)) (expt 10 (- 9 2))))
            (aeons          (floor (/ seconds 31557600 10 10 (expt 10 (- 9 2)))))
            (non-NIL ()))
        (flet ((p (i format) (when (< 0 i) (push (format NIL format i) non-NIL))))
          (p seconds "~a second~:p")
          (p minutes "~a minute~:p")
          (p hours "~a hour~:p")
          (p days "~a day~:p")
          (p weeks "~a week~:p")
          (p months "~a month~:p")
          (p years "~a year~:p")
          (p decades "~a decade~:p")
          (p centuries "~a centur~:@p")
          (p aeons "~a æon~:p")
          (format NIL "~{~a~^, ~}" non-NIL)))))

(define-page welcome "/" (:lquery (template "index.ctml"))
  (r-clip:process T))

(define-page not-found "/404" ()
  (error 'request-not-found))

(define-page denied "/403" ()
  (error 'request-denied))

(define-page internal-error "/500" ()
  (error 'internal-error))
