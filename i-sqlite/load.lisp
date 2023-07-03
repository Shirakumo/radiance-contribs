(in-package #:i-sqlite)

;; Load sqlite
(cond
  ((find-package "QL")
   (funcall (find-symbol "QUICKLOAD" "QL") :sqlite))
  ((find-package "ASDF")
   (funcall (find-symbol "LOAD-SYSTEM" "ASDF") :sqlite))
  (T (error "Neither Quicklisp nor ASDF could be found. What is going on?")))
