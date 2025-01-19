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

;; Two-arg helpers
(sb-c:defknown (two-arg-max two-arg-min) (real real) real
  (sb-c:movable sb-c:foldable sb-c:flushable))

(macrolet ((frob (name two-arg-op)
             `(sb-c:define-source-transform ,name (arg0 &rest rest)
                (if (null rest)
                    `(values (the real ,arg0))
                    (labels ((expand (arg0 &rest rest)
                               (if (null rest)
                                   arg0
                                   (sb-impl::once-only ((arg0 arg0)
                                                        (acc (apply #'expand rest)))
                                     `(,',two-arg-op ,acc ,arg0)))))
                      (apply #'expand arg0 rest))))))
  (frob max two-arg-max)
  (frob min two-arg-min))

(macrolet ((frob (name double-op single-op real-op)
             `(sb-c:deftransform ,name ((x y) (real real) *)
                (flet ((args-have-type-p (spec)
                         (let ((spec-type (sb-kernel:specifier-type spec)))
                           (or (sb-kernel:csubtypep (sb-c::lvar-type x) spec-type)
                               (sb-kernel:csubtypep (sb-c::lvar-type y) spec-type))))
                       (args-do-not-have-type-p (spec)
                         (let ((spec-type (sb-kernel:specifier-type spec)))
                           (and (eq (sb-kernel:type-intersection(sb-c::lvar-type x) spec-type)
                                    sb-kernel:*empty-type*)
                                (eq (sb-kernel:type-intersection(sb-c::lvar-type y) spec-type)
                                    sb-kernel:*empty-type*)))))
                  (cond
                    ((args-have-type-p 'double-float)
                     '(,double-op (sb-kernel:%double-float x) (sb-kernel:%double-float y)))
                    ((and (args-have-type-p 'single-float)
                          (args-do-not-have-type-p 'double-float))
                     '(,single-op (sb-kernel:%single-float x) (sb-kernel:%single-float y)))
                    (t
                     '(if (,real-op x y) x y)))))))
  (frob two-arg-min %mind %minf <)
  (frob two-arg-max %maxd %maxf >))
