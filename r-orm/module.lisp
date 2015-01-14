#|
This file is a part of Radiance
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:rad-user)
(define-module #:r-orm
  (:use #:cl #:radiance)
  (:export
   #:do-fields
   #:do-collect-fields
   #:collection
   #:_id
   #:fields
   #:collection
   #:table-data
   #:fetch
   #:fetch-one
   #:insert
   #:save
   #:purge
   #:define-collection))
