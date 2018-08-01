#|
 This file is a part of Radiance
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem #:r-oauth
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "oAuth 1.0a provider for Radiance."
  :module-name #:oauth
  :components ((:file "oauth")
               (:file "admin"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :session)
               :north-core
               :r-data-model
               :bordeaux-threads))
