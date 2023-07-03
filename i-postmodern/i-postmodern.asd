(asdf:defsystem #:i-postmodern
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "A bridging library to allow using Postmodern as a Radiance database."
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "connection")
               (:file "query")
               (:file "database"))
  :depends-on (:postmodern
               :bordeaux-threads))
