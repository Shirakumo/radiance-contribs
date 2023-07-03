(asdf:defsystem #:i-log4cl
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A briding library to allow using Log4CL as Radiance's logging implementation."
  :components ((:file "i-log4cl"))
  :depends-on (:log4cl))
