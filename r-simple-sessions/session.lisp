#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-sessions
  (:use #:cl #:radiance)
  (:implements #:session))
(in-package #:simple-sessions)

(defvar *session-table* (make-hash-table :test 'equalp))
(defvar *session-key* (make-random-string))
(defvar *session-timeout-format* '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))
(defvar *prune-thread* NIL)
(defvar *prune-interval* (* 60 5))

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
  (setf (gethash (id session) *session-table*) session)
  (trigger 'session:create session)
  ;; Trigger cookie creation
  (setf (session:timeout session)
        (timeout session)))

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
    (string (session:get session))))

(defun session:= (a b)
  (eql (ensure-session a)
       (ensure-session b)))

(defun session:start ()
  (let ((cookie (cookie "radiance-session")))
    (or (and cookie (decode-session cookie))
        (make-instance 'session))))

(defun session:list ()
  (loop for session being the hash-values of *session-table*
        collect session))

(defun session:get (session-id)
  (let ((session (gethash session-id *session-table*)))
    (when session
      (or (and (session:active-p session) session)
          (not (session:end session))))))

(defun session:id (session)
  (id session))

(defun session:field (session field)
  (gethash field (fields session)))

(defun (setf session:field) (value session field)
  (setf (gethash field (fields session)) value))

(defun session:timeout (session)
  (timeout session))

(defun (setf session:timeout) (seconds session)
  (setf (timeout session) seconds)
  ;; Update cookie
  (when (and (boundp '*request*) (boundp '*response*))
    (setf (cookie "radiance-session" :domain (domain *request*) :path "/" :timeout (timeout session) :http-only T)
          ;; Note: Add support for the secure flag through https options in the main framework
          (make-cookie-value session))))

(defun session:end (session)
  (v:debug :session "Ending session ~s" session)
  (setf (timeout session) 0)
  (remhash (id session) *session-table*)
  session)

(defun session:active-p (session)
  (and (< (get-universal-time) (timeout session))
       session))

(defun session::prune ()
  (v:info :session "Pruning dead sessions.")
  (maphash (lambda (uuid session)
             (unless (session:active-p session)
               (session:end session)))
           *session-table*))

(define-trigger request ()
  (setf *session* (session:start)))

(defun session::start-prune-thread ()
  (when *prune-thread*
    (error "Prune-thread already running."))
  (v:info :session "Starting prune thread.")
  (setf *prune-thread* (cons NIL T))
  (setf (car *prune-thread*)
        (bt:make-thread (lambda ()
                          (loop while (cdr *prune-thread*)
                                do (sleep *prune-interval*)
                                   (session::prune)))
                        :name "Session pruning thread")))

(defun session::stop-prune-thread (&optional force)
  (unless (or force *prune-thread*) 
    (error "No prune-thread running."))
  (v:info :session "Stopping prune thread.")
  (setf (cdr *prune-thread*) NIL)
  (cond
    ((not (car *prune-thread*)))
    (force
     (bt:destroy-thread (car *prune-thread*)))
    (T
     (bt:join-thread (car *prune-thread*))))
  (setf *prune-thread* NIL))

(define-trigger radiance:startup ()
  (session::start-prune-thread))

(define-trigger radiance:shutdown ()
  (session::stop-prune-thread))
