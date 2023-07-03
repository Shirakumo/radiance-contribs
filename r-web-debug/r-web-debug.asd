(asdf:defsystem #:r-web-debug
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "PRETTY-ERRORS"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Prints pretty and informative error pages."
  
  :components ((:file "pretty"))
  :depends-on (:dissect
               :r-clip
               :closer-mop))
