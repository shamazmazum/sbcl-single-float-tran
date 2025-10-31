(in-package :sbcl-single-float-tran)

(defun low-min (x y)
  (and x y (if (< x y) x y)))

(defun high-min (x y)
  (if (and x y)
      (if (< x y) x y)
      (or x y)))

(defun low-max (x y)
  (if (and x y)
      (if (> x y) x y)
      (or x y)))

(defun high-max (x y)
  (and x y (if (> x y) x y)))

(defun delistify (x)
  (if (atom x) x (first x)))

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

(defun min-max-result-type (low-ep-value high-ep-value
                            low-ep-type high-ep-type x y)
  (assert (eq (sb-kernel::numeric-type-format x)
              (sb-kernel::numeric-type-format y)))
  (let* ((x-interval (sb-c::type-approximate-interval x))
         (y-interval (sb-c::type-approximate-interval y))
         (low-x  (sb-c::interval-low  x-interval))
         (low-y  (sb-c::interval-low  y-interval))
         (high-x (sb-c::interval-high x-interval))
         (high-y (sb-c::interval-high y-interval))
         (%low   (funcall low-ep-value  (delistify low-x)  (delistify low-y)))
         (%high  (funcall high-ep-value (delistify high-x) (delistify high-y)))
         (low    (funcall low-ep-type  low-x  low-y  %low))
         (high   (funcall high-ep-type high-x high-y %high)))
    (sb-kernel:make-numeric-type
     :class  'float
     :format (sb-kernel::numeric-type-format x)
     :low    low
     :high   high)))

(sb-c:defoptimizer (%maxd sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-max #'high-max
   #'prefer-open-endpoint #'prefer-closed-endpoint
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%maxf sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-max #'high-max
   #'prefer-open-endpoint #'prefer-closed-endpoint
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%mind sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-min #'high-min
   #'prefer-closed-endpoint #'prefer-open-endpoint
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%minf sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-min #'high-min
   #'prefer-closed-endpoint #'prefer-open-endpoint
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))
