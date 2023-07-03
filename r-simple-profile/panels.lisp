(in-package #:simple-profile)

(profile:define-panel index (:user user :clip "panel-index.ctml")
  (r-clip:process
   T
   :user user))
