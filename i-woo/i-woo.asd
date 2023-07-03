(asdf:defsystem #:i-woo
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Bridging library to enable Woo as a Radiance server."
  :components ((:file "i-woo"))
  :depends-on (:woo
               :http-body
               :bordeaux-threads
               :uiop))
