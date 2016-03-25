(defsystem impressum
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :maintainer "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :version "0.1"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :depends-on (:cl-who
               :trivial-types)
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "components")
                 (:file "html")
                 (:file "print")
                 (:file "cl"))))
  :description "An interactive printer."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op impressum-test))))
