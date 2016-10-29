#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-errors
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-ERRORS"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Simple, plain error pages for Radiance."
  :components ((:file "error"))
  :depends-on (:r-clip))
