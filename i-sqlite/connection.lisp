(in-package #:i-sqlite)

(defvar *current-db* NIL)
(defvar *current-con* NIL)

(define-version-migration i-sqlite (NIL 1.1.0)
  (let ((config (mconfig-pathname #.*package*))
        (new (environment-module-directory #.*package* :data)))
    (dolist (file (uiop:directory* (make-pathname :type :wild :name :wild :defaults config)))
      (cond ((uiop:directory-pathname-p file)
             (rename-file file (merge-pathnames (make-pathname :directory `(:relative ,(last (pathname-directory file))))
                                                new)))
            ;; Copy only if not config file.
            ((not (string= (uiop:native-namestring file)
                           (uiop:native-namestring config)))
             (rename-file file (merge-pathnames (make-pathname :name (pathname-name file)
                                                               :type (pathname-type file))
                                                new)))))))

(defun wrangle-db-pathname (conn)
  (if (string= conn ":memory:")
      conn
      (uiop:native-namestring
       (merge-pathnames
        (etypecase conn
          (pathname conn)
          (string (uiop:parse-native-namestring conn)))
        (environment-module-directory #.*package* :data)))))

(defun db:connect (database-name)
  (with-simple-restart (skip "Skip connecting.")
    (flet ((err (msg) (error 'db:connection-failed :database database-name :message msg)))
      (let ((conn (config :connections database-name)))
        (unless conn (err "No such connection found."))
        (when *current-con*
          (warn 'db:connection-already-open :database database-name)
          (db:disconnect))
        ;; Spec restarts for already open.
        (l:info :database "Connecting ~a ~a" database-name conn)
        (let ((path (wrangle-db-pathname conn)))
          (ensure-directories-exist path)
          (setf *current-db* database-name
                *current-con* (sqlite:connect path)))
        (trigger 'db:connected database-name)))))

(defun db:disconnect ()
  (let ((database-name *current-db*))
    (l:info :database "Disconnecting ~a" database-name)
    (sqlite:disconnect *current-con*)
    (setf *current-con* NIL
          *current-db* NIL)
    (trigger 'db:disconnected database-name)))

(defun db:connected-p ()
  (not (null *current-con*)))
