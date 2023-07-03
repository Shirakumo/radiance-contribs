(in-package #:modularize-user)
(define-module #:data-model
  (:use #:cl)
  (:nicknames #:dm #:org.shirakumo.radiance.data-model)
  (:shadow #:get #:delete)
  (:export
   #:data-model
   #:id
   #:collection
   #:fields
   #:field
   #:get
   #:get-one
   #:hull
   #:hull-p
   #:save
   #:delete
   #:copy-model
   #:insert
   #:with-model-fields
   #:with-model
   #:with-model-save
   #:do-models))
