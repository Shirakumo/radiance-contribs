(asdf:defsystem #:i-lambdalite
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Bridging library to allow using LambdaLite as database for Radiance-"
  :components ((:file "module")
               (:file "database"))
  :depends-on (:lambdalite
               :cl-ppcre))
