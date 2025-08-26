(in-package :sbcl-single-float-tran)

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

(sb-c:defknown (%asinf)
    ((single-float -1f0 1f0)) (single-float #.(float (- (/ pi 2)) 0.0)
                                            #.(float (+ (/ pi 2)) 0.0))
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%acosf)
    ((single-float -1f0 1f0)) (single-float 0.0 #.(float pi 0.0))
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%atanf)
    (single-float) (single-float #.(float (- (/ pi 2)) 0.0)
                                 #.(float (+ (/ pi 2)) 0.0))
    (sb-c:movable sb-c:flushable sb-c:foldable)
  :overwrite-fndb-silently t)

(sb-c:defknown (%atan2f)
    (single-float single-float)
    (single-float #.(float (- pi) 0.0)
                  #.(float (+ pi) 0.0))
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

;; Min and max functions which translate to SSE instructions
#+x86-64
(progn
  (sb-c:defknown (%minf %maxf)
      (single-float single-float) single-float
      (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable)
    :overwrite-fndb-silently t)

  (sb-c:defknown (%mind %maxd)
      (double-float double-float) double-float
      (sb-c:movable sb-c:foldable sb-c:flushable sb-c:always-translatable)
    :overwrite-fndb-silently t)

  (sb-c:defknown (two-arg-max two-arg-min) (real real) real
      (sb-c:movable sb-c:foldable sb-c:flushable)
    :overwrite-fndb-silently t))
