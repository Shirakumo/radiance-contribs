(asdf:defsystem #:r-simple-auth
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "SIMPLE-AUTH"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A simple Radiance authentication interface implementation offering basic password logins."
  :components ((:file "auth"))
  :depends-on ((:interface :session)
               (:interface :user)
               :r-clip
               :r-ratify
               :crypto-shortcuts
               :do-urlencode))
