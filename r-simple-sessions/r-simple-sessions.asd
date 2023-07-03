(asdf:defsystem #:r-simple-sessions
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-SESSIONS"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "An implementation for Radiance's sessions interface using encrypted cookies for session tracking."
  :components ((:file "session"))
  :depends-on (:crypto-shortcuts
               :local-time
               :uuid
               :bordeaux-threads))
