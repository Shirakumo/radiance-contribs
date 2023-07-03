(in-package #:modularize-user)
(define-module #:simple-profile
  (:use #:cl #:radiance)
  (:implements #:profile)
  (:domain "user"))
(in-package #:simple-profile)

(define-trigger db:connected ()
  (db:create 'fields '((:name (:varchar 32))
                       (:type (:varchar 16))
                       (:default (:varchar 128))
                       (:editable (:integer 1)))
             :indices '(:name)))

(defun normalize (user &optional (errorp T))
  (etypecase user
    (user:user user)
    (null (user:get "anonymous"))
    ((or string symbol integer) (user:get user :if-does-not-exist (if errorp :error NIL)))))

(defun profile:avatar (user size)
  (let* ((user (normalize user NIL))
         (email (or (when user (user:field "email" user)) "")))
    (format NIL "//www.gravatar.com/avatar/~a?s=~d&d=blank"
            (cryptos:md5 (string-downcase email)) size)))

(defun profile:name (user-ish)
  (let ((user (normalize user-ish NIL)))
    (if user
        (or* (user:field "displayname" user)
             (user:username user))
        user-ish)))

(defun profile:fields ()
  (loop for model in (dm:get 'fields (db:query :all))
        collect `((:name . ,(dm:field model "name"))
                  (:type . ,(dm:field model "type"))
                  (:default . ,(dm:field model "default"))
                  (:editable . ,(= 1 (dm:field model "editable"))))))

(defun profile:add-field (name &key (type :text) default (editable T))
  (let ((name (string-downcase name)))
    (unless (db:select 'fields (db:query (:= 'name name)))
      (let ((type (string-downcase type)))
        (assert (member type '(text textarea password email url time date datetime datetime-local month week color number range checkbox radio file tel) :test #'string-equal)
                () "TYPE must be one of (text textarea password email url time date datetime datetime-local month week color number range checkbox radio file tel).")
        (db:insert 'fields `((name . ,name) (type . ,type) (default . ,(or default "")) (editable . ,(if editable 1 0)))))
      name)))

(defun profile:remove-field (name)
  (db:remove 'fields (db:query (:= 'name name))))

(defvar *panels* ())

(defclass panel ()
  ((name :initarg :name :accessor name)
   (access :initarg :access :accessor access)
   (function :initarg :func :accessor func)))

(defun profile::panel (name)
  (find name *panels* :key #'name :test #'string=))

(defun (setf profile::panel) (panel name)
  (profile:remove-panel name)
  (setf *panels* (sort (list* panel *panels*) #'string> :key #'name))
  panel)

(defun profile:remove-panel (name)
  (setf *panels* (remove name *panels* :key #'name :test #'string=))
  name)

(defun profile:list-panels ()
  (mapcar #'name *panels*))

(defmacro profile:define-panel (name options &body body)
  (let ((name (string-downcase name))
        (access (getf options :access)))
    (multiple-value-bind (body forms) (expand-options 'profile:panel options name body)
      (declare (ignore forms))
      `(setf (profile:panel ,name)
             (make-instance 'panel
                            :name ,name
                            :access ,access
                            :func (lambda (user-instance)
                                    (declare (ignorable user-instance))
                                    ,@body))))))

(define-option profile:panel :user (name body &optional var)
  (declare (ignore name))
  (if var
      `((let ((,var user-instance))
           ,@body))
      body))

(defun run-panel (panel user)
  (let ((panel (or (profile:panel panel)
                   (error 'request-not-found :message "No such panel."))))
    (let ((result (funcall (func panel) user)))
      (etypecase result
        (null "")
        (string result)
        (plump:node (plump:serialize result NIL))
        (array (lquery:$ result (serialize) (node)))))))

(define-page user-profile "user/([^/]+)?(/([^/]+))?" (:uri-groups (username NIL panel) :clip "public.ctml")
  (let ((user (when (and username (string/= "" username))
                (user:get username))))
    (if user
        (r-clip:process
         T
         :user user
         :you (or (auth:current) (user:get "anonymous"))
         :panels (loop for panel in *panels*
                       when (user:check (or (auth:current) (user:get "anonymous"))
                                        (access panel))
                       collect panel)
         :panel-name (or* panel "index")
         :panel (run-panel (or* panel "index") user))
        (error 'request-not-found :message (format NIL "No such user~@[ ~a~]." username)))))

(define-resource-locator profile page (user &optional tab)
  (make-uri :domains (list "user")
            :path (format NIL "~(~a~@[/~a~]~)" (user:username (or user "anonymous")) tab)))
