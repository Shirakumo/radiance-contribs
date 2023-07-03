(asdf:defsystem #:r-clip
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Trivial wrapper around lQuery and Clip to tighten Radiance integration."
  :components ((:file "package")
               (:file "pattern")
               (:file "clip-integration"))
  :depends-on (:clip))
