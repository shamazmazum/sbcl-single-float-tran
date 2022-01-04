(defsystem :sbcl-single-float-tran
  :name :sbcl-single-float-tran
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "SBC IR1 transformations for single-float math"
  :serial t
  :pathname "src/"
  :components (#+sbcl
               (:file "sbcl-transforms")
               #-sbcl
               (:file "no-sbcl")))
