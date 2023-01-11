(in-package :sbcl-single-float-tran)

;; Define stubs which translate to VOPS
(macrolet ((def-stubs (names)
             `(progn
                ,@(loop for name in names collect
                        `(defun ,name (x y)
                           (,name x y))))))
  (def-stubs (%minf %mind %maxf %maxd)))

;; Remove source transforms for min and max
(setf (sb-int:info :function :source-transform 'min) nil
      (sb-int:info :function :source-transform 'max) nil)
;; MIN and MAX share the same FUN-INFO for some reason,
;; so we need to redefine it
(sb-c:defknown (max min) (real &rest real) real
  (sb-c:movable sb-c:foldable sb-c:flushable)
  :overwrite-fndb-silently t)

;; NB: Order matters

;; Two arg transforms
(defun two-arg-min-max-transform-body (compare-op)
  (alex:with-gensyms (arg1 arg2)
    `(let ((,arg1 x)
           (,arg2 y))
       (if (,compare-op ,arg1 ,arg2)
           ,arg1 ,arg2))))

(macrolet ((def-min-max-transform (op compare-op)
             `(sb-c:deftransform ,op ((x y) (real real) *)
                (two-arg-min-max-transform-body ',compare-op))))
  (with-silent-transform-overwrite
    (def-min-max-transform min <=)
    (def-min-max-transform max >=)))

(macrolet ((def-min-max-transform (op single-op double-op)
             `(progn
                (sb-c:deftransform ,op ((x y) (single-float real) *)
                  '(,single-op x (coerce y 'single-float)))
                (sb-c:deftransform ,op ((x y) (real single-float) *)
                  '(,single-op (coerce x 'single-float) y))
                (sb-c:deftransform ,op ((x y) (double-float real) *)
                  '(,double-op x (coerce y 'double-float)))
                (sb-c:deftransform ,op ((x y) (real double-float) *)
                  '(,double-op (coerce x 'double-float) y)))))
  (with-silent-transform-overwrite
    (def-min-max-transform min %minf %mind)
    (def-min-max-transform max %maxf %maxd)))

;; Three arg transforms.
;;
;; FIXME: MIN and MAX on floating point numbers are not commutative
;; and associative (because of NaNs), but MIN and MAX on (AND REAL
;; (NOT FLOAT)) should be.
(defun three-arg-min-max-transform-body (compare-op)
  (alex:with-gensyms (arg1 arg2 arg3 tmp)
    `(let* ((,arg1 x)
            (,arg2 y)
            (,arg3 z)

            (,tmp (if (,compare-op ,arg1 ,arg2)
                      ,arg1 ,arg2)))
       (if (,compare-op ,tmp ,arg3)
           ,tmp ,arg3))))

(macrolet ((def-min-max-transform (op compare-op)
             `(sb-c:deftransform ,op ((x y z) (real real real) *)
                (three-arg-min-max-transform-body ',compare-op))))
  (with-silent-transform-overwrite
    (def-min-max-transform min <=)
    (def-min-max-transform max >=)))

(macrolet ((def-min-max-transform (op single-op double-op)
             `(progn
                (sb-c:deftransform ,op ((x y z) (single-float real real) *)
                  '(,single-op (,single-op x (coerce y 'single-float)) (coerce z 'single-float)))
                (sb-c:deftransform ,op ((x y z) (real single-float real) *)
                  '(,single-op (,single-op (coerce x 'single-float) y) (coerce z 'single-float)))
                (sb-c:deftransform ,op ((x y z) (real real single-float) *)
                  '(,single-op (,single-op (coerce x 'single-float) (coerce y 'single-float)) z))

                (sb-c:deftransform ,op ((x y z) (double-float real real) *)
                  '(,double-op (,double-op x (coerce y 'double-float)) (coerce z 'double-float)))
                (sb-c:deftransform ,op ((x y z) (real double-float real) *)
                  '(,double-op (,double-op (coerce x 'double-float) y) (coerce z 'double-float)))
                (sb-c:deftransform ,op ((x y z) (real real double-float) *)
                  '(,double-op (,double-op (coerce x 'double-float) (coerce y 'double-float)) z)))))
  (with-silent-transform-overwrite
    (def-min-max-transform min %minf %mind)
    (def-min-max-transform max %maxf %maxd)))

;; Four arguments and more are compiled as a regular function call.
