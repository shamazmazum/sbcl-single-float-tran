(defsystem :sbcl-single-float-tran
  :name :sbcl-single-float-tran
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "SBC IR1 transformations for single-float math"
  :licence "2-clause BSD"
  :serial t
  :pathname "src/"
  :components ((:file "sbcl-transforms" :if-feature :sbcl)
               (:file "no-sbcl" :if-feature (:not :sbcl))))
