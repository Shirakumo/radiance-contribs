(asdf:defsystem #:r-oauth
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "oAuth 1.0a provider for Radiance."
  :module-name #:oauth
  :components ((:file "oauth")
               (:file "documentation"))
  :depends-on ((:interface :database)
               (:interface :user)
               (:interface :auth)
               (:interface :session)
               :north-core
               :r-data-model
               :bordeaux-threads
               :documentation-utils))
