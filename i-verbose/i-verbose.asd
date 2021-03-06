#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#



(pushnew :verbose-no-init *features*)
(asdf:defsystem #:i-verbose
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A briding library to allow using Verbose as Radiance's logging implementation."
  :components ((:file "i-verbose"))
  :depends-on (:verbose))
