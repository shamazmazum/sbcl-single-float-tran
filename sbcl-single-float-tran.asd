(defsystem :sbcl-single-float-tran
  :name :sbcl-single-float-tran
  :version "0.5"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "SBCL IR1 transformations for single-float math (and more)"
  :licence "2-clause BSD"
  :serial t
  :pathname "src"
  :components ((:file "package"            :if-feature :sbcl)
               (:file "fndb"               :if-feature :sbcl)
               (:file "min-max-types"      :if-feature (:and :sbcl :x86-64))
               (:file "vop"                :if-feature (:and :sbcl :x86-64))
               (:file "transforms"         :if-feature :sbcl)
               (:file "irrat-transforms"   :if-feature :sbcl)
               (:file "min-max-transforms" :if-feature (:and :sbcl :x86-64))
               (:file "no-sbcl"            :if-feature (:not :sbcl)))
  :in-order-to ((test-op (load-op "sbcl-single-float-tran/tests")))
  :perform (test-op (op system)
                    (declare (ignore op system))
                    (funcall
                     (symbol-function
                      (intern (symbol-name '#:run-tests)
                              (find-package :sbcl-single-float-tran/tests))))))

(defsystem :sbcl-single-float-tran/tests
  :name :sbcl-single-float-tran/tests
  :version "0.5"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :licence "2-clause BSD"
  :serial t
  :pathname "tests"
  :components ((:file "package")
               (:file "tests"))
  :depends-on (:sbcl-single-float-tran :fiveam :serapeum :float-features))
