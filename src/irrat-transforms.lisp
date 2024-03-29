;; Stuff for optimizing math functions for single-float type.
;; Should be integrated in SBCL itself, but it's hard to do it right.
(in-package :sbcl-single-float-tran)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symbolicate (&rest args)
    (intern
     (apply
      #'concatenate
      'string
      (mapcar
       (lambda (x)
         (typecase x
           (string (string-upcase x))
           (symbol (symbol-name x))
           (t (error "Not a symbol or string"))))
       args)))))


;; Mathematical functions which call C library counterparts
;; (look at src/code/irrat.lisp in SBCL sources).
(macrolet
    ((def-alien (name nargs)
       (let ((func-name (symbolicate "%" name "f"))
             (alien-name (format nil "~af" name))
             (args (loop repeat nargs collect (gensym "ARG-"))))
         `(progn
            (declaim (inline ,func-name))
            (defun ,func-name ,args
              (sb-ext:truly-the
               ,(sb-kernel:type-specifier
                 (sb-kernel:fun-type-returns (sb-impl::info :function :type func-name)))
               (sb-alien:alien-funcall
                (sb-alien:extern-alien
                 ,alien-name
                 (function single-float ,@(loop repeat nargs collect 'single-float)))
                ,@args)))))))
  (def-alien "exp"  1)
  (def-alien "log"  1)
  (def-alien "sin"  1)
  (def-alien "cos"  1)
  (def-alien "tan"  1)
  (def-alien "sinh" 1)
  (def-alien "cosh" 1)
  #-x86-64
  (def-alien "sqrt" 1)
  (def-alien "tanh" 1)
  (def-alien "pow"  2))

;; Call VOP
#+x86-64
(defun %sqrtf (x)
  (%sqrtf x))

;; Define IR1 transformations from EXP to %EXP and so on.
;; (look at src/compiler/float-tran.lisp in SBCL source code).
(macrolet
    ((def-trans (name ret-type)
       (let ((trans-name (symbolicate "%" name "f"))
             (arg (gensym)))
         `(sb-c:deftransform ,name ((,arg) (single-float) ,ret-type)
            '(,trans-name ,arg)))))
  (with-silent-transform-overwrite
    (def-trans exp  *)
    (def-trans log  float)
    (def-trans sqrt float)
    (def-trans sin  *)
    (def-trans cos  *)
    (def-trans tan  *)
    (def-trans sinh *)
    (def-trans cosh *)
    (def-trans tanh *)))

(with-silent-transform-overwrite
  (sb-c:deftransform expt ((x y) (single-float single-float) single-float)
    '(%powf x y))
  (sb-c:deftransform expt ((x y) (single-float integer) single-float)
    '(%powf x (coerce y 'single-float))))
