(in-package #:modularize-user)
(define-module #:r-clip
  (:use #:cl #:radiance #:plump-lexer)
  (:export
   #:*document*
   #:process
   #:resolve-uri
   #:with-clip-processing
   #:switch-template))
(in-package #:r-clip)

(defpackage #:radiance-clip
  (:use #:cl #:radiance #:clip)
  (:import-from #:r-clip #:resolve-uri)
  (:shadow #:or*))
