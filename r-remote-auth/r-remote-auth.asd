#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:r-remote-auth
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :module-name "REMOTE-AUTH"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A Radiance authentication module using remote oAuth."
  :components ((:file "auth"))
  :depends-on ((:interface :session)
               (:interface :user)
               :r-clip
               :south))
