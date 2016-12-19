#|
 This file is a part of Radiance
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-lambdalite
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Bridging library to allow using LambdaLite as database for Radiance-"
  :components ((:file "module")
               (:file "database"))
  :depends-on (:lambdalite
               :cl-ppcre))
