#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:r-data-model
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "DATA-MODEL"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "Trivially simple Radiance database record wrapper"
  :components ((:file "module")
               (:file "model"))
  :depends-on ((:interface :database)))
