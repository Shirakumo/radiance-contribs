(asdf:defsystem #:r-ratify
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Bridge to integrate ratify into radiance and provide additional checks."
  :components ((:file "ratify"))
  :depends-on (:ratify
               :crypto-shortcuts))
