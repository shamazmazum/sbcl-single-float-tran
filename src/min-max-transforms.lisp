(in-package :sbcl-single-float-tran)

;; Remove source transforms for min and max
(setf (sb-int:info :function :source-transform 'min) nil
      (sb-int:info :function :source-transform 'max) nil)

;; MIN and MAX share the same FUN-INFO for some reason,
;; so we need to redefine it
(sb-c:defknown (max min) (real &rest real) real
  (sb-c:movable sb-c:foldable sb-c:flushable)
  :overwrite-fndb-silently t)

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

(defun types-dont-intersect-p (&rest types)
  (eq (apply #'sb-kernel:type-intersection types) sb-kernel:*empty-type*))

(macrolet ((frob (name double-op single-op real-op)
             `(progn
                ;; Required for constant folding
                (defun ,name (x y)
                  ;; M is used for number contagion
                  (let ((m (* (if (typep x 'float) (float 1 x) 1)
                              (if (typep y 'float) (float 1 y) 1))))
                    (* (if (,real-op x y) x y) m)))

                (sb-c:deftransform ,name ((x y) (real real) *)
                  (flet ((args-have-type-p (spec)
                           (let ((spec-type (sb-kernel:specifier-type spec)))
                             (or (sb-kernel:csubtypep (sb-c::lvar-type x) spec-type)
                                 (sb-kernel:csubtypep (sb-c::lvar-type y) spec-type))))
                         (args-do-not-have-type-p (spec)
                           (let ((spec-type (sb-kernel:specifier-type spec)))
                             (and (types-dont-intersect-p (sb-c::lvar-type x) spec-type)
                                  (types-dont-intersect-p (sb-c::lvar-type y) spec-type)))))
                    (cond
                      ((args-have-type-p 'double-float)
                       '(,double-op (sb-kernel:%double-float x) (sb-kernel:%double-float y)))
                      ((and (args-have-type-p 'single-float)
                            (args-do-not-have-type-p 'double-float))
                       '(,single-op (sb-kernel:%single-float x) (sb-kernel:%single-float y)))
                      (t
                       '(if (,real-op x y) x y))))))))
  (frob two-arg-min %mind %minf <)
  (frob two-arg-max %maxd %maxf >))
