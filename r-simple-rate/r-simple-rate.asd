(asdf:defsystem #:r-simple-rate
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-RATE"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance rate interface implementation offering a convenient db-backed rate limiting."
  :components ((:file "rate"))
  :depends-on ((:interface :database)
               :r-data-model))
