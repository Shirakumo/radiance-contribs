(asdf:defsystem #:i-hunchentoot
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.1.0"
  :description "Bridging library to enable Hunchentoot as a Radiance interface."
  :components ((:file "i-hunchentoot"))
  :depends-on (:hunchentoot
               :cl-ppcre
               :ironclad))
