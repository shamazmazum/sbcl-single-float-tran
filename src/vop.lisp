(in-package :sb-vm)

(define-vop (fsqrtf)
  (:args (x :scs (single-reg)))
  (:results (y :scs (single-reg)))
  (:translate sbcl-transforms::%sqrtf)
  (:policy :fast-safe)
  (:arg-types single-float)
  (:result-types single-float)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 1
     (unless (location= x y)
       (inst xorps y y))
     (note-float-location 'sqrt vop x)
     (inst sqrtss y x)))
