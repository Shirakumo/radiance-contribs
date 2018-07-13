#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:r-simple-profile
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-PROFILE"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
