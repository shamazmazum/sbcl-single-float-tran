(in-package :sbcl-single-float-tran)

(defun delistify (x)
  (if (atom x) x (first x)))

(defun interval-op (f x y)
  (funcall f (delistify x) (delistify y)))

(defun prefer-closed-endpoint (sx sy s)
  (when s
    (let ((%sx (delistify sx))
          (%sy (delistify sy)))
      (if (or (and sx sy
                   (= %sx %sy s)
                   (or (atom sx)
                       (atom sy)))
              (and sx (= s %sx)
                   (atom sx))
              (and sy (= s %sy)
                   (atom sy)))
          s (list s)))))

(defun prefer-open-endpoint (sx sy s)
  (when s
    (let ((%sx (delistify sx))
          (%sy (delistify sy)))
      (if (or (and sx sy
                   (= %sx %sy s)
                   (or (listp sx)
                       (listp sy)))
              (and sx (= s %sx)
                   (listp sx))
              (and sy (= s %sy)
                   (listp sy)))
          (list s) s))))

(defun interval-min (x y)
  (let* ((xlow  (sb-c::interval-low x))
         (xhigh (sb-c::interval-high x))
         (ylow  (sb-c::interval-low y))
         (yhigh (sb-c::interval-high y))
         (low (and xlow ylow
                   (interval-op
                    (lambda (x y)
                      (if (< x y) x y))
                    xlow ylow)))
         (high (if (and xhigh yhigh)
                   (interval-op
                    (lambda (x y)
                      (if (< x y) x y))
                    xhigh yhigh)
                   (delistify
                    (or xhigh yhigh)))))
    (sb-c::make-interval :low  (prefer-closed-endpoint xlow  ylow  low)
                         :high (prefer-open-endpoint   xhigh yhigh high))))

(defun interval-max (x y)
  (let* ((xlow  (sb-c::interval-low x))
         (xhigh (sb-c::interval-high x))
         (ylow  (sb-c::interval-low y))
         (yhigh (sb-c::interval-high y))
         (low (if (and xlow ylow)
                  (interval-op
                   (lambda (x y)
                     (if (> x y) x y))
                   xlow ylow)
                  (delistify
                   (or xlow ylow))))
         (high (and xhigh yhigh
                    (interval-op
                     (lambda (x y)
                       (if (> x y) x y))
                     xhigh yhigh))))
    (sb-c::make-interval :low  (prefer-open-endpoint   xlow  ylow  low)
                         :high (prefer-closed-endpoint xhigh yhigh high))))

(macrolet ((def-type-deriver (name interval-fn)
             `(defun ,name (x y samep)
                (if samep x
                    (let* ((x-interval (sb-c::numeric-type->interval x))
                           (y-interval (sb-c::numeric-type->interval y))
                           (result (,interval-fn x-interval y-interval))
                           (result-type (sb-c::numeric-contagion x y)))
                      ;; If the result type is a float, we need to be sure to coerce
                      ;; the bounds into the correct type.
                      (when (eq (sb-kernel:numeric-type-class result-type) 'float)
                        (setf result (sb-c::interval-func
                                      #'(lambda (x)
                                          (sb-c::coerce-for-bound
                                           x (or (sb-kernel:numeric-type-format result-type)
                                                 'float)))
                                      result)))
                      (sb-kernel:make-numeric-type
                       :class  (sb-kernel:numeric-type-class  result-type)
                       :format (sb-kernel:numeric-type-format result-type)
                       :low    (sb-c::interval-low  result)
                       :high   (sb-c::interval-high result)))))))
  (def-type-deriver min-derive-type-aux interval-min)
  (def-type-deriver max-derive-type-aux interval-max))


(sb-c:defoptimizer (two-arg-min sb-c:derive-type) ((x y))
  (sb-c::two-arg-derive-type x y #'min-derive-type-aux))

(sb-c:defoptimizer (two-arg-max sb-c:derive-type) ((x y))
  (sb-c::two-arg-derive-type x y #'max-derive-type-aux))

(sb-c:defoptimizer (two-arg-min sb-c::constraint-propagate-result) ((x y))
  (list (list '<= x)
        (list '<= y)))

(sb-c:defoptimizer (two-arg-max sb-c::constraint-propagate-result) ((x y))
  (list (list '>= x)
        (list '>= y)))
