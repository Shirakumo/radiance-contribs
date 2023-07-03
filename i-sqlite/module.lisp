(in-package #:modularize-user)
(define-module #:i-sqlite
  (:use #:cl #:radiance)
  (:implements #:database #:relational-database)
  (:export
   #:*sqlite-pcre-paths*))
