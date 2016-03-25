;;;; Impressum print methods
(in-package :impressum)

(defgeneric print-object-briefly (object)
  (:method ((object t))
    "The default method. Returns @c(NIL)."
    nil)
  (:documentation "Return a brief description of an object."))

(defgeneric print-object (object)
  (:method ((object t))
    "The default print method."
    (make-text (prin1-to-string (type-of object))
               :tags (list "unknown")))
  (:documentation "Return a complete description of an object."))

(defun print (object &optional (stream *standard-output*) (backend :html))
  "Print an object. If the object has a brief representation, print it, but with
a widget to expand it to the full representation."
  (let ((brief (print-object-briefly object)))
    (render (if brief
                (make-instance 'more
                               :brief brief
                               :full (print-object object))
                (print-object object))
            stream
            :backend backend)
    object))

(defun print-to-string (object &optional (backend :html))
  "Print an object to a string."
  (with-output-to-string (stream)
    (print object stream backend)))
