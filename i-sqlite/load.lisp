#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

;; Load sqlite
(cond
  ((find-package "QL")
   (funcall (find-symbol "QUICKLOAD" "QL") :sqlite))
  ((find-package "ASDF")
   (funcall (find-symbol "LOAD-SYSTEM" "ASDF") :sqlite))
  (T (error "Neither Quicklisp nor ASDF could be found. What is going on?")))
