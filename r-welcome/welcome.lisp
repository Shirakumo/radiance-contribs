(in-package #:modularize-user)
(define-module #:welcome
    (:use #:cl #:radiance))
(in-package #:welcome)

(define-page welcome "/" (:clip "index.ctml")
  (r-clip:process T :request *request*))

(define-page not-found "/404" ()
  (error 'request-not-found))

(define-page denied "/403" ()
  (error 'request-denied))

(define-page internal-error "/500" ()
  (error 'internal-error))

(define-trigger user:ready ()
  (unless (user:get "welcome" :if-does-not-exist NIL)
    (user:get "welcome" :if-does-not-exist :create)
    (user:grant "welcome" "*")
    (format T "~&~%;; Created a default user called \"welcome\"")
    (when (find-package "SIMPLE-AUTH")
      (auth::set-password "welcome" "welcome")
      (format T " with the password \"welcome\"."))
    (format T "~%")))

(format T "~&~%
;;;; You are loading R-WELCOME
;; This most likely means that this is your first time running
;; Radiance. Welcome! Once the loading has finished please visit
;;
;;    http://localhost:8080/
;;
;; with your browser to see a demo page of a running system. If
;; you would like to get rid of this module, either edit the
;; configuration file for the ~a environment at
;; ~a
;; or start Radiance with a different environment, and thus your
;; own configuration entirely.
~%"
        (environment) (uiop:native-namestring ubiquitous:*storage-pathname*))
