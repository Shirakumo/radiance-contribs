(asdf:defsystem #:r-simple-ban
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-BAN"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance ban interface implementation offering a simple banning mechanism."
  :components ((:file "ban"))
  :depends-on (:r-clip))
