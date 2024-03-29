(asdf:defsystem #:i-json
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A bridge library to cl-json to allow JSON API format output."
  :components ((:file "i-json"))
  :depends-on (:cl-json
               :closer-mop))
