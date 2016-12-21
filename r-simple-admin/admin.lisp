#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-admin
  (:use #:cl #:radiance)
  (:implements #:admin)
  (:domain "admin"))
(in-package #:simple-admin)

(defvar *panels* ())

(define-resource-locator admin page (&optional category panel &rest args)
  (declare (ignore args))
  (cond (category
         (make-uri :domains '("admin")
                   :path (format NIL "~(/~a/~a~)" category panel)))
        (T
         #@"admin/")))

(defclass menu-entry ()
  ((name :initarg :name :accessor name)
   (icon :initarg :icon :accessor icon)
   (tooltip :initarg :tooltip :accessor tooltip)
   (access :initarg :access :accessor access))
  (:default-initargs
   :name (error "NAME required.")
   :icon "fa-folder"
   :tooltip ""
   :access ()))

(defmethod print-object ((entry menu-entry) stream)
  (print-unreadable-object (entry stream :type T)
    (format stream "~a" (name entry))))

(defclass category (menu-entry)
  ((panels :initarg :panels :accessor panels))
  (:default-initargs
   :panels ()))

(defmethod print-object ((category category) stream)
  (print-unreadable-object (category stream :type T)
    (format stream "~a ~a" (name category) (mapcar #'name (panels category)))))

(defclass panel (menu-entry)
  ((category :initarg :category :accessor category)
   (func :initarg :func :accessor func))
  (:default-initargs
   :category (error "CATEGORY required.")
   :func (error "FUNC required.")))

(defmethod print-object ((panel panel) stream)
  (print-unreadable-object (panel stream :type T)
    (format stream "~a/~a" (category panel) (name panel))))

(defun admin::category (category)
  (find category *panels* :key #'name :test #'string=))

(defun (setf admin::category) (category name)
  (admin::remove-category name)
  (setf *panels* (sort (list* category *panels*)
                       #'string< :key #'name))
  category)

(defun admin::remove-category (category)
  (setf *panels* (remove category *panels* :key #'name :test #'string=))
  category)

(defun admin::panel (category name)
  (let ((category (admin::category category)))
    (when category
      (find name (panels category) :key #'name :test #'string=))))

(defun (setf admin::panel) (panel category name)
  (let ((category (or (admin::category category)
                      (setf (admin::category category) (make-instance 'category :name category)))))
    (setf (panels category) (sort (list* panel (remove name (panels category) :key #'name :test #'string=))
                                  #'string< :key #'name))
    panel))

(defun admin:remove-panel (category name)
  (let ((category (admin::category category)))
    (when category
      (setf (panels category) (remove name (panels category) :key #'name :test #'string=))
      (unless (panels category)
        (admin::remove-category category)))
    name))

(defmacro admin:define-panel (category name options &body body)
  (let ((name (string-downcase name))
        (category (string-downcase category))
        (options (copy-list options)))
    (destructuring-bind (&key (icon "") (tooltip "") (access ()) &allow-other-keys) options
      (remf options :icon)
      (remf options :tooltip)
      (multiple-value-bind (body forms) (expand-options 'admin:panel options name body category)
        (declare (ignore forms))
        `(setf (admin::panel ,category ,name)
               (make-instance 'panel
                              :name ,name
                              :category ,category
                              :icon ,icon
                              :tooltip ,tooltip
                              :access ,access
                              :func (lambda () ,@body)))))))

(defun run-panel (category panel)
  (let ((panel (admin::panel category panel)))
    (when panel
      (let ((result (funcall (func panel))))
        (etypecase result
          (null "")
          (string result)
          (plump:node (plump:serialize result NIL))
          (array (lquery:$ result (serialize) (node))))))))

(define-page admin-index "admin/([^/]*)(/(.+))?" (:uri-groups (category NIL panel) :access () :clip "index.ctml")
  (let ((manage (post/get "simple-admin-manage"))
        (action (post-var "simple-admin-action")))
    (when (string= "" category)
      (setf category "admin" panel "overview"))
    (r-clip:process
     T
     :manage manage
     :category category
     :panel panel
     :panels (loop for category in *panels*
                   when (user:check (auth:current) (access category))
                   collect `(:name ,(name category)
                             :panels ,(loop for panel in (panels category)
                                            when (user:check (auth:current) (access panel))
                                            collect panel)))
     :content (or (when (and manage (user:check (auth:current) (perm radiance admin)))
                    (cond ((not action)
                           (plump:parse (@template "confirm.ctml")))
                          ((not (string-equal action "yes"))
                           NIL)
                          ((string= manage "shutdown")
                           (bt:make-thread #'(lambda () (sleep 1) (radiance:shutdown)))
                           "Shutting down.")
                          ((string= manage "restart")
                           (bt:make-thread #'(lambda () (sleep 1) (radiance:shutdown) (radiance:startup)))
                           "Restarting.")))
                  (run-panel category panel)))))
