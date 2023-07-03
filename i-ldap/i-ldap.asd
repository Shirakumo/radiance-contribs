(asdf:defsystem #:i-ldap
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance authentication implementation using an LDAP backend."
  :serial T
  :components ((:file "ldap")
               (:file "frontend"))
  :depends-on ((:interface :session)
               :r-clip
               :r-ratify
               :trivial-ldap
               :crypto-shortcuts
               :lispqr))
