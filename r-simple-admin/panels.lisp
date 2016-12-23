#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-admin)

(defun print-symbol (symb)
  (format NIL "~a~:[::~;:~]~a"
          (package-name (symbol-package symb))
          (eql :external (nth-value 1 (find-symbol (symbol-name symb) (symbol-package symb))))
          (symbol-name symb)))

(admin:define-panel admin overview (:access () :icon "fa-home" :tooltip "Radiance overview info.")
  (r-clip:process
   (plump:parse
    (if (user:check (or (auth:current) (user:get "anonymous"))
                    (perm radiance admin))
        (@template "overview.ctml")
        (@template "overview-public.ctml")))))

(admin:define-panel admin modules (:access (perm radiance admin modules) :icon "fa-cube" :tooltip "Oversee active modules.")
  (let* ((action (post-var "action"))
         (selected (post-var "selected[]"))
         (module (post-var "module"))
         (modules (if module (cons module selected) selected))
         (error NIL) (info NIL))
    (when (string-equal action "delete")
      (handler-case
          (progn (dolist (module modules)
                   (l:warn :simple-admin "Deleting module ~a as per front-end request." module)
                   (modularize:delete-module module))
                 (setf info (format NIL "Deleted modules ~{~a~^, ~}" modules)))
        (error (err)
          (setf error (princ-to-string err)))))
    
    (r-clip:process
     (plump:parse (@template "modules.ctml"))
     :error error
     :info info
     :modules (remove-if #'interfaces:interface-p (modularize:list-modules)))))

(admin:define-panel admin systems (:access (perm radiance admin systems) :icon "fa-briefcase" :tooltip "Manage ASDF systems.")
  (let* ((action (post-var "action"))
         (selected (post-var "selected[]"))
         (system (post-var "system"))
         (systems (if system (cons system selected) selected))
         (error NIL) (info NIL))
    (handler-case
        (cond ((string-equal action "reload")
               (dolist (system systems)
                 (l:info :simple-admin "Reloading system ~a as per front-end request." system)
                 (asdf:load-system system))
               (setf info (format NIL "Reloaded systems ~{~a~^, ~}" systems)))
              ((string-equal action "load")
               (l:info :simple-admin "Attempting to load system ~a as per front-end request." system)
               (asdf:load-system system)
               (setf info (format NIL "System ~a loaded." system)))
              ((string-equal action "quickload")
               (l:info :simple-admin "Attempting to quickload system ~a as per front-end request." system)
               (ql:quickload system)
               (setf info (format NIL "System ~a quickloaded." system))))
      (error (err)
        (setf error (princ-to-string err))))
    
    (r-clip:process
     (plump:parse (@template "systems.ctml"))
     :error error
     :info info
     :systems (asdf:already-loaded-systems))))

(admin:define-panel admin dispatchers (:access (perm radiance admin dispatchers) :icon "fa-at" :tooltip "Manage Radiance's dispatchers.")
  (let* ((action (post-var "action"))
         (selected (post-var "selected[]"))
         (dispatcher (post-var "dispatcher"))
         (dispatchers (if dispatcher (cons dispatcher selected) selected))
         (error NIL) (info NIL))
    (handler-case
        (cond ((string-equal action "remove")
               (dolist (dispatcher dispatchers)
                 (let ((dispatcher (let ((*read-eval* NIL))
                                     (read-from-string dispatcher))))
                   (l:info :simple-admin "Removing dispatcher ~s as per front-end request." dispatcher)
                   (remove-uri-dispatcher dispatcher)))
               (setf info (format NIL "Removed dispatchers ~{~a~^, ~}" dispatchers))))
      (error (err)
        (setf error (princ-to-string err))))
    
    (r-clip:process
     (plump:parse (@template "dispatchers.ctml"))
     :error error
     :info info
     :dispatchers (list-uri-dispatchers))))
