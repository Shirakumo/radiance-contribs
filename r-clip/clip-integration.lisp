(in-package #:r-clip)

(defvar *document*)

(defun radiance-clip::or* (&rest vals)
  (loop for val in vals
        thereis (if (stringp val)
                    (unless (string= val "") val)
                    val)))

(defun process (target &rest fields)
  (process* target (apply #'clip:make-clipboard fields)))

(defun process* (target clipboard)
  (let ((*package* (find-package "RADIANCE-CLIP")))
    (clip:process*
     (etypecase target
       ((eql T) *document*)
       (pathname (plump:parse target))
       (string (plump:parse target))
       (plump:node target))
     clipboard)))

(defmacro with-clip-processing ((template &optional (content-type "application/xhtml+xml; charset=utf-8")) &body body)
  (let ((result (gensym "RESULT")))
    `(let ((*document* (plump:parse ,(if (stringp template)
                                         `(@template ,template)
                                         template))))
       (flet ((,result ()
                ,@body))
         (let ((,result (,result)))
           (typecase ,result
             (plump:node
              (setf (content-type *response*) ,content-type)
              (handler-bind ((plump:invalid-xml-character #'abort)
                             (plump:discouraged-xml-character #'muffle-warning))
                (let ((plump:*tag-dispatchers* ,(if (search "xhtml" content-type)
                                                    'plump:*xml-tags*
                                                    'plump:*html-tags*)))
                  (plump:serialize ,result NIL))))
             (T ,result)))))))

(defmacro switch-template (template)
  `(setf *document* (lquery:load-page (@template ,template))))

(defun transform-body (body template)
  (if template
      `((with-clip-processing ,(radiance::enlist template)
          ,@body))
      body))

(define-option radiance:page :clip (name body uri &optional template)
  (declare (ignore name uri))
  (transform-body body template))

(define-option admin:panel :clip (name body category &optional template)
  (declare (ignore name category))
  (transform-body body template))

(define-option profile:panel :clip (name body &optional template)
  (declare (ignore name))
  (transform-body body template))

(macrolet ((define-pattern-attribute (name)
             (let ((symb (intern (concatenate 'string "@" (string name)))))
               `(clip:define-attribute-processor ,symb (node value)
                  (plump:remove-attribute node ,(string-downcase symb))
                  (when (< 0 (length value))
                    (setf (plump:attribute node ,(string-downcase name))
                          (parse-uri* value)))))))
  (define-pattern-attribute href)
  (define-pattern-attribute src)
  (define-pattern-attribute link)
  (define-pattern-attribute action)
  (define-pattern-attribute formaction))

(lquery:define-lquery-function time (node time &optional (format :human))
  (if time
      (let ((stamp (etypecase time
                     (local-time:timestamp time)
                     ((eql T) (local-time:now))
                     (fixnum (local-time:universal-to-timestamp time))
                     (real (local-time:universal-to-timestamp (round time)))
                     (string (local-time:parse-timestring time)))))
        (setf (plump:attribute node "datetime")
              (format-machine-date stamp))
        (setf (plump:attribute node "title")
              (format-fancy-date stamp))
        (setf (plump:children node) (plump:make-child-array))
        (plump:make-text-node node (ecase format
                                     (:relative (format-relative-time stamp))
                                     (:year (local-time:format-timestring NIL stamp :format '(:year)
                                                                                    :timezone local-time:+utc-zone+))
                                     (:date (local-time:format-timestring NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2))
                                                                                    :timezone local-time:+utc-zone+))
                                     (:clock (format-clock-time stamp))
                                     (:human (format-human-date stamp))
                                     (:machine (format-machine-date stamp))
                                     (:fancy (format-fancy-date stamp)))))
      (setf (plump:children node) (plump:make-child-array)))
  node)

(lquery:define-lquery-function select (node value)
  (let ((value (typecase value
                 (string value)
                 (T (princ-to-string value)))))
    (loop for child across (plump:children node)
          do (when (and (typep child 'plump:element)
                        (string= (plump:tag-name child) "option")
                        (string= (plump:attribute child "value") value))
               (setf (plump:attribute child "selected") "selected"))))
  node)

;; We need to do this ugly hack in order to defer it to when the system is actually loaded.
;; This is necessary to bypass a nasty dependency on the database interface through the
;; data-model interface, when it really isn't necessary to have it.
(defun add-clip-data-model-method ()
  (let ((*package* #.*package*))
    (eval (read-from-string "(defmethod clip:clip ((object dm:data-model) field)
                               (dm:field object field))"))))

(when (asdf:component-loaded-p (asdf:find-system :r-data-model))
  (add-clip-data-model-method))

(defmethod asdf:perform :after ((op asdf:load-op) (c (eql (asdf:find-system :r-data-model))))
  (add-clip-data-model-method))
