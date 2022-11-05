;; Stuff for optimizing math functions for single-float type.
;; Should be integrated in SBCL itself, but it's hard to do it right.
(defpackage sbcl-transforms
  (:use :cl))
(in-package :sbcl-transforms)

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

;; Tell the compiler math functions are pure
;; (look at src/compiler/generic/vm-fndb.lisp in SBCL source code).

;; Additional hack is needed for these functions to be really flushable:
;; https://sourceforge.net/p/sbcl/mailman/message/37134684/
(sb-c:defknown (%expf %sqrtf %coshf)
    (single-float) (single-float 0f0)
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%sinf %cosf %tanhf)
    (single-float) (single-float -1f0 1f0)
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%logf %tanf %sinhf)
    (single-float) single-float
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%powf)
  (single-float single-float) single-float
    (sb-c:movable sb-c:foldable sb-c:flushable)
  :overwrite-fndb-silently t)

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
  (def-trans exp  *)
  (def-trans log  float)
  (def-trans sqrt float)
  (def-trans sin  *)
  (def-trans cos  *)
  (def-trans tan  *)
  (def-trans sinh *)
  (def-trans cosh *)
  (def-trans tanh *))

(sb-c:deftransform expt ((x y) (single-float single-float) single-float)
  `(%powf x y))

(sb-c:deftransform expt ((x y) (single-float integer) single-float)
  `(%powf x (coerce y 'single-float)))
