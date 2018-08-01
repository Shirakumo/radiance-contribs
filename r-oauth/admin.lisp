#|
 This file is a part of Radiance
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:oauth)

(define-implement-trigger admin
  (admin:define-panel oauth applications (:access (perm oauth application) :clip "application.ctml" :icon "fa-rocket")
    (r-clip:process
     T :applications (dm:get 'applications (db:query (:= 'author (user:id (auth:current)))))))

  (admin:define-panel oauth authorizations (:access (perm oauth authorize) :clip "authorizations.ctml" :icon "fa-key")
    (r-clip:process
     T :applications (loop for data in (db:select 'sessions (db:query (:= 'user (user:id (auth:current)))) :fields '(key))
                           collect (dm:get-one 'applications (db:query (:= 'key (gethash "key" data))))))))
