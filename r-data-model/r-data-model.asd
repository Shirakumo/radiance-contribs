(asdf:defsystem #:r-data-model
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "DATA-MODEL"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "Trivially simple Radiance database record wrapper"
  :components ((:file "module")
               (:file "model"))
  :depends-on ((:interface :database)))
