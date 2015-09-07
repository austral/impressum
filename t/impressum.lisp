(in-package :cl-user)
(defpackage impressum-test
  (:use :cl :fiveam))
(in-package :impressum-test)

(def-suite tests
  :description "impressum tests.")
(in-suite tests)

(test simple-test
  (is
   (equal 1 1))
  (is-true
   (and t t)))

(run! 'tests)
