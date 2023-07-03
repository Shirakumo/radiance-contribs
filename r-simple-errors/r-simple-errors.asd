(asdf:defsystem #:r-simple-errors
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-ERRORS"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Simple, plain error pages for Radiance."
  :components ((:file "error"))
  :depends-on (:r-clip))
