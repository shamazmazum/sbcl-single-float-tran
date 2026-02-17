(error "Can't build sb-single-float-tran with ASDF")

(defsystem :sb-single-float-tran
  :name "SB-SINGLE-FLOAT-TRAN"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :version "1.0"
  :description "SBCL IR1 transformations for single-float math (and more)"
  :licence "2-clause BSD"
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "fndb")
     (:file "min-max-types"      :if-feature :x86-64)
     (:file "vop"                :if-feature :x86-64)
     (:file "transforms")
     (:file "irrat-transforms")
     (:file "min-max-transforms" :if-feature :x86-64)))))
