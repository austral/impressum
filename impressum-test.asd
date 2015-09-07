(defsystem impressum-test
  :author "Fernando Borretti <eudoxiahp@gmail.com>"
  :license "MIT"
  :depends-on (:impressum
               :fiveam)
  :components ((:module "t"
                :serial t
                :components
                ((:file "impressum")))))
