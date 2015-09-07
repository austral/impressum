;;;; Impressum print methods
(in-package :impressum)

(defgeneric print-object-briefly (object)
  (:documentation "Return a brief description of an object."))

(defgeneric print-object (object)
  (:documentation "Return a complete description of an object."))

(defun print (object &optional (stream *standard-output*) (backend :html))
  "Print an object."
  (render (print-object object) stream :backend backend)
  object)

(defun print-to-string (object &optional (backend :html))
  "Print an object to a string."
  (with-output-to-string (stream)
    (print object stream backend)))

;;; Print methods for Common Lisp types

(defmethod print-object ((object integer))
  "Print an integer."
  (make-context-info
   (make-text (write-to-string object)
              :tags (list "integer"))
   (with-group (:direction :vertical)
     (make-text (format nil "Hexadecimal: ~X" object)))))
