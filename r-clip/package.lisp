#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
  (:shadow #:or*))
