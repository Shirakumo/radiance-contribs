(asdf:defsystem #:r-welcome
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "WELCOME"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A basic welcome page for new Radiance setups."
  :serial T
  :components ((:file "welcome"))
  :depends-on (:r-clip))
