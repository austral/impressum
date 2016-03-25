(in-package :cl-user)
(defpackage impressum
  (:use :cl)
  (:shadow :print-object :print)
  (:import-from :trivial-types
                :proper-list)
  ;; Components
  (:export :component)
  ;; Component constructors
  (:export :make-text
           :make-group
           :with-group
           :make-box
           :with-box
           :make-context-info)
  (:export :render
           :print-object-briefly
           :print-object
           :print
           :print-to-string))
