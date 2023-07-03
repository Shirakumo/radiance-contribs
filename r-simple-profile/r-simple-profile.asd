(asdf:defsystem #:r-simple-profile
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-PROFILE"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Simple user profiles implementation for Radiance."
  :components ((:file "profile")
               (:file "admin")
               (:file "panels"))
  :depends-on ((:interface :user)
               :r-data-model
               :crypto-shortcuts
               :r-clip
               :ratify))
