(in-package :impressum)

;;; HTML Renderer

(defgeneric render-html (component stream)
  (:documentation "Render a component to an HTML stream."))

(defmethod render-html ((component text) stream)
  "Render a text component"
  (who:with-html-output (stream)
    (who:htm (:span :class (format nil "~{~A~^ ~}" (tags component))
               (who:str (text component))))))

(defun layout-to-string (component)
  "Return a string matching a direction object."
  (ecase (direction component)
    (:horizontal
     "horizontal")
    (:vertical
     "vertical")))

(defun tags-to-class (component-type component)
  "Turn a list of style tags into a string, the resulting HTML tag's class. The
@cl:param(component-type) argument is a string designating the type of
component, e.g. 'group' or 'tooltip'."
  (format nil "~{~A~^ ~}" (cons component-type
                                (append
                                 (if (typep component 'layout-mixin)
                                     (list (layout-to-string component)))
                                 (if (typep component 'style-mixin)
                                     (tags component))))))

(defmethod render-html ((component children-mixin) stream)
  "Render children mixin. Called using @cl:spec(call-next-method)."
  (loop for child in (children component) do
    (render-html child stream)))

(defmethod render-html ((component group) stream)
  "Render a group component."
  (who:with-html-output (stream)
    (who:htm
     (:span :class (tags-to-class "group" component)
       (call-next-method)))))

(defmethod render-html ((component box) stream)
  "Render a box component."
  (who:with-html-output (stream)
    (who:htm
     (:div :class (tags-to-class "box" component)
       (call-next-method)))))

(defmethod render-html ((component context-info) stream)
  "Render a tooltip component."
  (who:with-html-output (stream)
    (who:htm
     (:div :class (tags-to-class "context" component)
       (:div :class "context-object"
         (render-html (object component) stream))
       (:div :class "context-extra"
         (render-html (extra component) stream))))))

(defmethod render-html ((component more) stream)
  "Render a more component."
  (who:with-html-output (stream)
    (who:htm
     (:div :class (tags-to-class "more" component)
       (:div :class "brief"
         (render-html (brief component) stream))
       (:div :class "full"
         (render-html (full component) stream))))))

(defun print-html (component stream)
  "Print a component to a stream as HTML."
  (who:with-html-output (stream)
    (who:htm (:div :class "impressum-output"
               (render-html component stream))))
  component)

(defun render (component stream &key (backend :html))
  "Render @cl:param(component) to the @cl:param(stream), using the backend
@cl:param(backend)."
  (ecase backend
    (:html
     (print-html component stream))))
