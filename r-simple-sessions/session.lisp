(in-package #:modularize-user)
(define-module #:simple-sessions
  (:use #:cl #:radiance)
  (:implements #:session))
(in-package #:simple-sessions)

(defvar *session-table* (make-hash-table :test 'equalp
                                         #+(or sbcl ecl) :synchronized
                                         #+(or sbcl ecl) T))
(defvar *session-key* (make-random-string))
(defvar *session-timeout-format* '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
(defvar *prune-thread* NIL)
(defvar *prune-interval* (* 60 60))

(defclass session (session:session)
  ((id :initarg :id :initform (princ-to-string (uuid:make-v4-uuid)) :accessor id)
   (fields :initarg :fields :initform (make-hash-table :test 'eql) :accessor fields)
   (timeout :initarg :timeout :initform (+ (get-universal-time) session:*default-timeout*) :accessor timeout)))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type T)
    (format stream "~a " (id session))
    (local-time:format-timestring stream (local-time:universal-to-timestamp (timeout session)) :format *session-timeout-format*)))

(defun make-cookie-value (session)
  (cryptos:encrypt (format NIL "~a-~a" (id session) (make-random-string (+ 4 (random 9)))) *session-key*))

(defmethod initialize-instance :after ((session session) &key)
  (l:debug :session "Starting session ~a" session)
  (when (boundp '*request*)
    (setf (gethash 'session (data *request*)) session)))

(defun decode-session (hash)
  (ignore-errors
   (let ((hash (cryptos:decrypt hash *session-key*)))
     (when (< 36 (length hash))
       (let ((session (session:get (subseq hash 0 36))))
         (when session
           (l:debug :session "Resuming session ~a" session)
           session))))))

(defun ensure-session (session)
  (etypecase session
    (session:session session)
    (string (gethash session *session-table*))
    (null (if (boundp '*request*)
              (or (gethash 'session (data *request*))
                  (let ((cookie (or (cookie "radiance-session")
                                    (post/get "radiance-session"))))
                    (or (and cookie (decode-session cookie))
                        (make-instance 'session))))
              (make-instance 'session)))))

(defun session:= (a b)
  (eql (ensure-session a)
       (ensure-session b)))

(defun session:start (&optional session)
  (let ((session (or session (ensure-session session))))
    (unless (gethash (id session) *session-table*)
      (setf (gethash (id session) *session-table*) session)
      (trigger 'session:create session)
      ;; Trigger cookie creation
      (setf (session:timeout session)
            (timeout session)))
    session))

(defun session:list ()
  (loop for session being the hash-values of *session-table*
        collect session))

(defun session:get (&optional session-id)
  (let ((session (ensure-session session-id)))
    (when session
      (if (session:active-p session)
          session
          (null (session:end session))))))

(defun session:id (&optional session)
  (id (ensure-session session)))

(defun session:field (session/field &optional (field NIL field-p))
  (if field-p
      (gethash field (fields (ensure-session session/field)))
      (gethash session/field (fields (ensure-session NIL)))))

(defun (setf session:field) (value session/field &optional (field NIL field-p))
  (let ((session (if field-p (ensure-session session/field) (ensure-session NIL))))
    (session:start session)
    (setf (gethash (if field-p field session/field) (fields session)) value)))

(defun session:timeout (&optional session)
  (timeout (ensure-session session)))

(defun (setf session:timeout) (seconds &optional session)
  (let ((session (ensure-session session)))
    (setf (timeout session) seconds)
    ;; Update cookie
    (when (and (boundp '*request*) (boundp '*response*))
      (session:start session)
      ;; Attempt to set a cookie for the root domain
      (if (find #\. (domain *request*))
          (setf (cookie "radiance-session" :path "/" :timeout (timeout session) :http-only T
                                           :domain (format NIL ".~a" (domain *request*)))
                ;; Note: Add support for the secure flag through https options in the main framework
                (make-cookie-value session))
          (setf (cookie "radiance-session" :path "/" :timeout (timeout session) :http-only T)
                ;; Note: Add support for the secure flag through https options in the main framework
                (make-cookie-value session))))))

(defun session:end (&optional session)
  (let ((session (ensure-session session)))
    (l:debug :session "Ending session ~s" session)
    (setf (timeout session) 0)
    (remhash (id session) *session-table*)
    session))

(defun session:active-p (&optional session)
  (let ((session (ensure-session session)))
    (and (< (get-universal-time) (timeout session))
         (gethash (id session) *session-table*))))

(defun session::persist ()
  (l:info :session "Persisting sessions.")
  (let ((tmp (environment-module-pathname #.*package* :cache "sessions.tmp")))
    (ubiquitous:with-local-storage (tmp)
      (let ((table (make-hash-table :test 'equalp)))
        (loop for key being the hash-keys of *session-table* using (hash-value session)
              for copy = (make-instance 'session :id (id session) :timeout (timeout session))
              do (setf (gethash key table) copy)
                 ;; Only copy the string or number keys since they don't depend on packages
                 (loop for key being the hash-keys of (fields session) using (hash-value value)
                       do (unless (typep value '(or standard-object structure-object))
                            (setf (gethash key (fields copy)) value))))
        (setf (ubiquitous:value :sessions) table)))
    (rename-file tmp (make-pathname :type "lisp" :defaults tmp))))

(defun session::restore ()
  (l:info :session "Restoring sessions.")
  (let ((tmp (environment-module-pathname #.*package* :cache "sessions.lisp")))
    (ubiquitous:with-local-storage (tmp)
      (when (ubiquitous:value :sessions)
        (loop for session being the hash-values of (ubiquitous:value :sessions)
              do (when (session:active-p session)
                   (setf (gethash (id session) *session-table*) session)))))))

(defun session::prune ()
  (l:info :session "Pruning dead sessions.")
  (maphash (lambda (uuid session)
             (unless (session:active-p session)
               (session:end session)))
           *session-table*))

(defun session::start-prune-thread ()
  (when *prune-thread*
    (error "Prune-thread already running."))
  (l:info :session "Starting prune thread.")
  (setf *prune-thread* (cons NIL T))
  (setf (car *prune-thread*)
        (bt:make-thread (lambda ()
                          (catch 'exit
                            (loop while (cdr *prune-thread*)
                                  do (sleep *prune-interval*)
                                     (session::prune)
                                     (session::persist))))
                        :name "Session pruning thread")))

(defun session::stop-prune-thread (&key kill)
  (unless (or kill *prune-thread*) 
    (error "No prune-thread running."))
  (l:info :session "Stopping prune thread.")
  (setf (cdr *prune-thread*) NIL)
  (cond
    ((not (car *prune-thread*)))
    (kill
     (bt:destroy-thread (car *prune-thread*)))
    (T
     (bt:interrupt-thread (car *prune-thread*)
                          (lambda () (throw 'exit NIL)))
     (ignore-errors (bt:join-thread (car *prune-thread*)))))
  (setf *prune-thread* NIL))

(define-trigger radiance:startup-done ()
  (unless *prune-thread*
    (handler-case (session::restore)
      (error (e)
        (l:error "Failed to restore sessions: ~a" e)))
    (session::start-prune-thread)))

(define-trigger radiance:shutdown ()
  (when *prune-thread*
    (session::stop-prune-thread)
    (session::persist)))
