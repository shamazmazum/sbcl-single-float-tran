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

(defun min-max-result-type (low high x y)
  (assert (eq (sb-kernel::numeric-type-format x)
              (sb-kernel::numeric-type-format y)))
  (let ((low-x  (sb-kernel:numeric-union-type-low  x))
        (low-y  (sb-kernel:numeric-union-type-low  y))
        (high-x (sb-kernel:numeric-union-type-high x))
        (high-y (sb-kernel:numeric-union-type-high y)))
    (sb-kernel:make-numeric-type
     :class  'float
     :format (sb-kernel::numeric-type-format x)
     :low    (funcall low  low-x  low-y)
     :high   (funcall high high-x high-y))))

(sb-c:defoptimizer (%maxd sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-max #'high-max
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%maxf sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-max #'high-max
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%mind sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-min #'high-min
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))

(sb-c:defoptimizer (%minf sb-c:derive-type) ((x y))
  (min-max-result-type
   #'low-min #'high-min
   (sb-c::lvar-type x)
   (sb-c::lvar-type y)))
