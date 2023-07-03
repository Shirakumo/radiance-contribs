(asdf:defsystem #:i-sqlite
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.1.0"
  :description "A bridging library to allow using SQLite3 as a Radiance database."
  :components ((:file "module")
               (:file "load")
               (:file "extension")
               (:file "connection")
               (:file "query")
               (:file "toolkit")
               (:file "database"))
  :depends-on (:cffi))
