(asdf:defsystem #:r-simple-cache
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-CACHE"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance cache interface implementation offering a convenient and simple disk-file-based caching mechanism."
  :components ((:file "cache"))
  :depends-on (:uiop
               :alexandria))
