#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(asdf:defsystem #:i-wookie
  :defsystem-depends-on (:radiance)
  :class "radiance:virtual-module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "A bridging library to allow using the Wookie webserver as Radiance's server implementation."
  :components ((:file "server"))
  :depends-on (:wookie
               :cl-ppcre
               :bordeaux-threads
               :do-urlencode
               :alexandria))
