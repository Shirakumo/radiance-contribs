#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:r-clip)

;;;;;;
;; PATTERN syntax
;;
;; pattern   ::= ([domain] [port] "/" [path]) | resource
;; domain    ::= (subdomain ".")* subdomain
;; subdomain ::= alphas | subst
;; port      ::= ":" (number | subst)
;; path      ::= (string | subst)*
;; resource  ::= "<" target [":" name [";" args]] ">"
;; target    ::= alphas
;; name      ::= alphas
;; args      ::= arg*
;; arg       ::= subst | resource | ((!">"|",")*)
;; subst     ::= "{" (!"}")* "}"

(defvar *args* ())
(define-matcher letters (or (in #\a #\z) (in #\A #\Z)))
(define-matcher number (in #\0 #\9))
(define-matcher alpha (or :letters :number (is #\-)))

(defclass pattern (uri)
  ())

(defgeneric resolve (pattern &rest args)
  (:method (thing &rest args)
    (declare (ignore args))
    thing)
  (:method ((pattern pattern) &rest args)
    (let ((*args* (or args *args*)))
      (make-uri :domains (mapcar #'resolve (domains pattern))
                :port (resolve (port pattern))
                :path (if (listp (path pattern))
                          (format NIL "~{~a~}" (mapcar #'resolve (path pattern)))
                          (resolve (path pattern)))))))

(defun parse-pattern (string)
  (with-lexer-environment (string)
    (list* (read-pattern)
           (let ((start plump:*index*))
             (loop until (<= (length plump:*string*) start)
                   for (token new-start) = (multiple-value-list
                                            (read-from-string plump:*string* T T :start start))
                   collect (clip:resolve-value token)
                   do (setf start new-start))))))

(defun read-pattern ()
  (or (read-resource)
      (let ((domains (read-domains))
            (port (read-port))
            (path (read-path)))
        (make-instance
         'pattern :domains domains :port port :path path :matcher NIL))))

(defun read-domains ()
  (loop with domains = ()
        while (funcall (make-matcher (or (any #\. #\< #\{) :alpha)))
        do (when (char= (peek) #\.) (advance))
           (push (read-subdomain) domains)
        finally (return domains)))

(defun read-subdomain ()
  (or (read-substitute)
      (consume-until (make-matcher (not :alpha)))))

(defun read-port ()
  (when (char= (or (peek) #\ ) #\:)
    (advance) ;; skip beginning :
    (or (read-substitute)
        (consume-until (make-matcher (not :number))))))

(defun read-path ()
  (when (or (char= (or (peek) #\ ) #\/)
            (error "Path / expected."))
    (advance) ;; skip beginning /
    (loop for peek = (peek)
          while peek
          collect (case peek
                    (#\{ (read-substitute))
                    (T (consume-until (make-matcher (is #\{))))))))

(defclass resource ()
  ((target :initarg :target :initform (error "TARGET required.") :accessor target)
   (name :initarg :name :initform :domain :accessor name)
   (args :initarg :args :initform () :accessor args)))

(defmethod resolve ((resource resource) &rest args)
  (let ((*args* (or args *args*)))
    (apply #'resource (target resource) (name resource) (mapcar #'resolve (args resource)))))

(defun read-resource ()
  (when (char= (or (peek) #\ ) #\<)
    (advance) ;; skip opening <
    (let ((module (read-resource-target))
          (name (read-resource-name))
          (args (read-resource-args)))
      (advance) ;; skip closing >
      (unless (module-p module)
        (warn "No module or interface ~a known, but used as resource identifier in URI." module))
      (make-instance 'resource :target module :name (or* name :domain) :args args))))

(defun read-resource-name ()
  (when (char= (peek) #\:)
    (advance)
    (consume-until (make-matcher (any #\> #\;)))))

(defun read-resource-target ()
  (string-upcase (consume-until (make-matcher (any #\> #\:)))))

(defun read-resource-args ()
  (when (char= (peek) #\;)
    (advance)
    (loop for peek = (peek)
          until (or (not peek) (char= peek #\>))
          do (when (char= peek #\,) (advance))
          collect (or (read-substitute)
                      (read-resource)
                      (consume-until (make-matcher (any #\> #\,)))))))

(defclass placeholder ()
  ((var :initarg :var :initform (error "VAR required.") :accessor var)))

(defmethod resolve ((placeholder placeholder) &rest args)
  (declare (ignore args))
  (let ((var (var placeholder)))
    (etypecase var
      (fixnum (nth var *args*))
      (keyword (getf *args* var)))))

(defun read-substitute ()
  (when (char= (or (peek) #\ ) #\{)
    (advance) ;; skip opening {
    (let* ((contents (consume-until (make-matcher (is #\}))))
           (keyword (or (ignore-errors (parse-integer contents))
                        (intern (string-upcase contents) "KEYWORD"))))
      (advance) ;; skip closing }
      (make-instance 'placeholder :var keyword))))
