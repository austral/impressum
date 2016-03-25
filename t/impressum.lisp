(in-package :cl-user)
(defpackage impressum-test
  (:use :cl :fiveam))
(in-package :impressum-test)

(def-suite tests
  :description "impressum tests.")
(in-suite tests)

(test cl-types
  (macrolet ((is-component (object)
               `(is
                 (typep (impressum:print-object ,object) 'impressum:component))))
    (is-component 1)
    (is-component 3.14)
    (is-component #\a)
    (is-component "test")
    (is-component 'a)
    (is-component :a)))

(run! 'tests)
