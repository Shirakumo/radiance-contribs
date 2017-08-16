#|
 This file is a part of TyNETv5/Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:r-welcome
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "WELCOME"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A basic welcome page for new Radiance setups."
  :serial T
  :components ((:file "welcome"))
  :depends-on (:r-clip))
