(asdf:defsystem #:r-simple-users
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-USERS"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "A simple implementation for Radiance's users interface, offering database-backed user storage."
  :components ((:file "users"))
  :depends-on ((:interface :database)
               :r-data-model
               :cl-ppcre))
