(asdf:defsystem #:r-simple-admin
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-ADMIN"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.1.0"
  :description "A simple Radiance administration interface implementation."
  :serial T
  :components ((:file "admin")
               (:file "panels"))
  :depends-on ((:interface :auth)
               :r-clip
               :alexandria))
