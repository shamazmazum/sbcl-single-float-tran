(in-package :sbcl-single-float-tran)

(defmacro with-silent-transform-overwrite (&body body)
  `(handler-bind
       ((sb-kernel:redefinition-with-deftransform #'muffle-warning))
     ,@body))
