(in-package #:simple-profile)

(define-trigger user:ready ()
  (user:add-default-permissions (perm profile change)))

(define-implement-trigger admin
  (admin:define-panel settings account (:access (perm profile change account) :clip "account.ctml" :icon "fa-user" :tooltip "Change account information.")
    (let ((user (auth:current))
          (fields (dm:get 'fields (db:query (:= 'editable 1)))))
      (with-actions (error info)
          ((:save
            (ratify:perform-test
              :email (post-var "email"))
            (setf (user:field "displayname" user) (post-var "displayname")
                  (user:field "email" user) (post-var "email"))
            (dolist (field fields)
              (let ((val (post-var (dm:field field "name"))))
                (ratify:perform-test (find-symbol (string-upcase (dm:field field "type")) :keyword) val)
                (setf (user:field (dm:field field "name") user)
                      (if (or (not val) (string= val ""))
                          (dm:field field "default")
                          val))))
            (setf info "Account updated.")))
        (r-clip:process
         T
         :error error :info info
         :user user
         :fields fields))))

  (admin:define-panel settings profile (:access (perm profile change profile) :clip "profile.ctml" :icon "fa-home" :tooltip "Configure your profile looks.")
    (let ((user (auth:current)))
      (with-actions (error info)
          ((:save
            (ratify:perform-combined-tests
              (:color (post-var "color"))
              (:property (post-var "background"))
              (:boolean (post-var "show-actions")))
            (setf (user:field "simple-profile-color" user) (post-var "color")
                  (user:field "simple-profile-background" user) (post-var "background")
                  (user:field "simple-profile-actions" user) (post-var "show-actions"))
            (setf info "Profile updated.")))
        (r-clip:process
         T
         :info info :error error
         :user user))))

  (admin:define-panel users fields (:access (perm radiance admin users fields) :clip "fields.ctml" :icon "fa-list" :tooltip "Set user profile fields.")
    (with-actions (error info)
        ((:add
          (db:insert 'fields `((name . ,(post-var "name")) (type . ,(post-var "type")) (default . ,(post-var "default")) (editable . ,(parse-integer (or* (post-var "editable") "0"))))))
         (:delete
          (dolist (name (or (post-var "selected[]") (list (post-var "name"))))
            (db:remove 'fields (db:query (:= 'name name))))))
      (r-clip:process
       T
       :error error
       :fields (dm:get 'fields (db:query :all))))))
