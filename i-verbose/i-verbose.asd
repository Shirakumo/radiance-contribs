(pushnew :verbose-no-init *features*)
(asdf:defsystem #:i-verbose
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A briding library to allow using Verbose as Radiance's logging implementation."
  :components ((:file "i-verbose"))
  :depends-on (:verbose))
