(asdf:defsystem #:i-sendmail
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "A sendmail implementation for the mail interface."
  :serial T
  :components ((:file "sendmail"))
  :depends-on (:cl-sendmail))
