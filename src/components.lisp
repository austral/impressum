(in-package :impressum)

;;; Components
;;;
;;; To print an object, Impressum builds a tree of components which are then
;;; rendered to HTML (or some other format).

;;; Base classes

(defclass component ()
  ()
  (:documentation "The base class of all components."))

(defclass children-mixin ()
  ((children :reader children
             :initarg :children
             :type (proper-list component)
             :initform nil
             :documentation "A list of child components."))
  (:documentation "A component with children."))

(defclass style-mixin ()
  ((tags :reader tags
         :initarg :tags
         :type (proper-list string)
         :initform nil
         :documentation "A list of style tags."))
  (:documentation "A mixin to style components."))

(deftype direction ()
  "A direction for layouts, either @c(:horizontal) or @c(:vertical)."
  `(or (eql :horizontal) (eql :vertical)))

(defclass layout-mixin ()
  ((direction :reader direction
              :initarg :direction
              :type direction
              :documentation "The direction."))
  (:documentation "A mixin to arrange components in some layout."))

;;; Actual rendered elements

(defclass text (component style-mixin)
  ((text :reader text
         :initarg :text
         :type string
         :documentation "The actual text."))
  (:documentation "A text node."))

(defclass group (component children-mixin style-mixin layout-mixin)
  ()
  (:default-initargs :direction :horizontal)
  (:documentation "A component to group elements in."))

(defclass box (component children-mixin style-mixin layout-mixin)
  ()
  (:default-initargs :direction :vertical)
  (:documentation "A component that groups sub-components in a way that
  separates them from sibling components (e.g., a border)."))

(defclass context-info (component)
  ((object :reader object
           :initarg :object
           :type component
           :documentation "The component the information is associated to.")
   (extra :reader extra
          :initarg :extra
          :type component
          :documentation "The component with the extra information."))
  (:documentation "Represents a component that has extra contextual information."))

;;; Constructors

(defun make-text (text &key tags)
  (make-instance 'text
                 :tags tags
                 :text text))

(defun make-group (&key tags direction children)
  (make-instance 'group
                 :tags tags
                 :direction direction
                 :children children))

(defmacro with-group ((&key tags direction) &body children)
  `(make-group :tags ,tags :direction ,direction :children (list ,@children)))

(defun make-box (&key tags direction children)
  (make-instance 'box
                 :tags tags
                 :direction direction
                 :children children))

(defmacro with-box ((&key tags direction) &body children)
  `(make-box :tags ,tags :direction ,direction :children (list ,@children)))

(defun make-context-info (object extra)
  (make-instance 'context-info
                 :object object
                 :extra extra))
