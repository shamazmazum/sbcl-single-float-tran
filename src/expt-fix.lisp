(in-package :sb-c)

;; SBCL has the following invalid transform for expt in
;; src/compiler/float-tran.lisp:
;;
;;(deftransform expt ((x y) (single-float integer) double-float)
;;  `(coerce (%pow (coerce x 'double-float) (coerce y 'double-float))
;;    'single-float))
;;
;; This transform is never applied, surely, but causes much trouble
;; when I create a transform with the correct type, namely
;; (FUNCTION (SINGLE-FLOAT INTEGER) SINGLE-FLOAT).
;; This is because the order in which transforms are stored in the
;; list matters and the new transform added on top of the list "kills"
;; optimizations like (EXPT X) => (* X X). In order to keep good
;; optimizations I change the type to the correct one and later
;; overwrite this transform in sbcl-transforms.lisp

;; Fixed in SBCL 2.2.1
;; The following lines do nothing on newer versions.

(flet ((patch-transform-type (function old-type new-type)
         (let ((info (fun-info-or-lose function)))
           (with-accessors ((transforms fun-info-transforms))
               info
             (let ((transform (find (specifier-type old-type)
                                    transforms
                                    :test #'type=
                                    :key  #'transform-type)))
               (when transform
                 (setf (transform-type transform)
                       (specifier-type new-type))))))
         (values)))
  (patch-transform-type
   'expt
   '(function (single-float integer) double-float)
   '(function (single-float integer) single-float)))
