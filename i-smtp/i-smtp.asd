(asdf:defsystem #:i-smtp
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "An SMTP-backed implementation for the mail interface."
  :serial T
  :components ((:file "smtp"))
  :depends-on (:cl-smtp))
