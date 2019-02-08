#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:i-hunchentoot
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.1.0"
  :description "Bridging library to enable Hunchentoot as a Radiance interface."
  :components ((:file "i-hunchentoot"))
  :depends-on (:hunchentoot
               :cl-ppcre
               :ironclad))
