(asdf:defsystem #:r-remote-auth
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "REMOTE-AUTH"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A Radiance authentication module using remote oAuth."
  :components ((:file "auth"))
  :depends-on ((:interface :session)
               (:interface :user)
               :r-clip
               :south))
