(in-package :sbcl-single-float-tran/tests)

(defun run-tests (&key (which :all))
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 (ecase which
                   (:all          '(type-derivation precise imprecise))
                   (:precise-only '(type-derivation precise))))))

(def-suite precise
    :description "Test transforms which give precisely the same result as before")
(def-suite imprecise
    :description "Test transforms which give almost the same result as before")
(def-suite type-derivation
    :description "Check type derivers for some functions")

(in-suite precise)
(defmacro %define-min/max (name op (&rest arg-types) res-type)
  (let ((variables (loop repeat (length arg-types) collect (gensym "VAR-"))))
    `(progn
       (serapeum:-> ,name ,arg-types (values ,res-type &optional))
       (defun ,name ,variables
         (,op ,@variables)))))

(defmacro define-min/max (name (&rest arg-types) res-type)
  (let ((min-name (intern (concatenate 'string "MIN-" (symbol-name name))))
        (max-name (intern (concatenate 'string "MAX-" (symbol-name name)))))
    `(progn
       (%define-min/max ,min-name min ,arg-types ,res-type)
       (%define-min/max ,max-name max ,arg-types ,res-type))))

;; Compiles to MINSD
(define-min/max single/double/fixnum (single-float double-float fixnum) double-float)
;; Compiles to comparison+MINSD or casts and MINSDs
(define-min/max double/real/fixnum (double-float real fixnum) double-float)
;; Compiles to comparisons
(define-min/max single/real/fixnum (single-float real fixnum) real)
(define-min/max single/single (single-float single-float) single-float)

(test min
  (flet ((comp-min (x y z)
           (let ((min (if (< x y) x y)))
             (if (< min z) min z))))
    (loop repeat 100000
          for x = (random 1000)
          for y = (random 1000.0)
          for z = (random 1000d0) do
          (is (= (min-double/real/fixnum z y x)
                 (comp-min z y x)))
          (is (= (min-single/real/fixnum y z x)
                 (comp-min y z x)))
          (is (= (min-single/double/fixnum y z x)
                 (comp-min y z x))))))

(test min-special
  ;; SBCL's min/max return the first argument
  (is (eq (min-single/single -0.0  0.0) -0.0))
  (is (eq (min-single/single  0.0 -0.0)  0.0))
  (float-features:with-float-traps-masked (:invalid)
    (is (eq (min-single/single float-features:single-float-nan 3.0)
            float-features:single-float-nan))
    (is (eq (min-single/single 3.0 float-features:single-float-nan)
            3.0))))

(test max
  (flet ((comp-max (x y z)
           (let ((min (if (> x y) x y)))
             (if (> min z) min z))))
    (loop repeat 100000
          for x = (random 1000)
          for y = (random 1000.0)
          for z = (random 1000d0) do
          (is (= (max-double/real/fixnum z y x)
                 (comp-max z y x)))
          (is (= (max-single/real/fixnum y z x)
                 (comp-max y z x)))
          (is (= (max-single/double/fixnum y z x)
                 (comp-max y z x))))))

(test max-special
  ;; SBCL's min/max return the first argument
  (is (eq (max-single/single -0.0  0.0) -0.0))
  (is (eq (max-single/single  0.0 -0.0)  0.0))
  (float-features:with-float-traps-masked (:invalid)
    (is (eq (max-single/single float-features:single-float-nan 3.0)
            float-features:single-float-nan))
    (is (eq (max-single/single 3.0 float-features:single-float-nan)
            3.0))))


(in-suite imprecise)
(defconstant +limit+ 10000000.0)

(defmacro def-imprecise-test (name precision &key
                                               (single-type 'single-float)
                                               (real-type   'real)
                                               (random-form '(random +limit+)))
  (let ((single-name (intern (concatenate 'string (symbol-name name) "-SINGLE")))
        (real-name   (intern (concatenate 'string (symbol-name name) "-REAL"))))
  `(progn
     (serapeum:-> ,single-name (,single-type) (values single-float &optional))
     (defun ,single-name (x)
       (,name x))

     (serapeum:-> ,real-name (,real-type) (values real &optional))
     (defun ,real-name (x)
       (,name x))

     (test ,name
       (loop repeat 500000
             for x = ,random-form do
             (is (< (abs (- (,real-name x) (,single-name x))) ,precision)))))))

;; TODO: Check others
(def-imprecise-test log 1f-5
  :single-type (single-float 1.0)
  :real-type (real 1)
  :random-form (1+ (random +limit+)))
(def-imprecise-test sin 1f-6)
(def-imprecise-test cos 1f-6)

(in-suite type-derivation)

(defun min-deriver (x y)
  (sbcl-single-float-tran::min-derive-type-aux x y nil))

(defun max-deriver (x y)
  (sbcl-single-float-tran::max-derive-type-aux x y nil))

(test min-derive-type
  (is (min-deriver
       (sb-kernel:specifier-type '(single-float * *))
       (sb-kernel:specifier-type '(single-float -1.0 2.0)))
      (sb-kernel:specifier-type '(single-float * *)))
  (is (min-deriver
       (sb-kernel:specifier-type '(single-float -10.0 *))
       (sb-kernel:specifier-type '(single-float * 2.0)))
      (sb-kernel:specifier-type '(single-float * 2.0)))
  (is (min-deriver
       (sb-kernel:specifier-type '(single-float -10.0 1.0))
       (sb-kernel:specifier-type '(single-float -20.0 2.0)))
      (sb-kernel:specifier-type '(single-float -20.0 1.0)))
  (is (min-deriver
       (sb-kernel:specifier-type '(single-float -10.0 1.0))
       (sb-kernel:specifier-type '(double-float -20d0 2d0)))
      (sb-kernel:specifier-type '(double-float -20d0 1d0)))
  (is (min-deriver
       (sb-kernel:specifier-type '(double-float -10d0 2d0))
       (sb-kernel:specifier-type '(single-float (-10.0) (2.0))))
      (sb-kernel:specifier-type '(double-float -10d0 (2d0))))
  (is (min-deriver
       (sb-kernel:specifier-type '(integer (-10) 2))
       (sb-kernel:specifier-type '(single-float -8.0 4.0)))
      (sb-kernel:specifier-type '(single-float -9.0 2.0))))

(test max-derive-type
  (is (max-deriver
       (sb-kernel:specifier-type '(single-float * *))
       (sb-kernel:specifier-type '(single-float -1.0 2.0)))
      (sb-kernel:specifier-type '(single-float * *)))
  (is (max-deriver
       (sb-kernel:specifier-type '(single-float -10.0 *))
       (sb-kernel:specifier-type '(single-float * 2.0)))
      (sb-kernel:specifier-type '(single-float -10.0 *)))
  (is (max-deriver
       (sb-kernel:specifier-type '(single-float -10.0 1.0))
       (sb-kernel:specifier-type '(single-float -20.0 2.0)))
      (sb-kernel:specifier-type '(single-float -10.0 2.0)))
  (is (max-deriver
       (sb-kernel:specifier-type '(single-float -10.0 1.0))
       (sb-kernel:specifier-type '(double-float -20d0 2d0)))
      (sb-kernel:specifier-type '(double-float -10d0 2d0)))
  (is (max-deriver
       (sb-kernel:specifier-type '(double-float -10d0 2d0))
       (sb-kernel:specifier-type '(single-float (-10.0) (2.0))))
      (sb-kernel:specifier-type '(double-float (-10d0) 2d0)))
  (is (max-deriver
       (sb-kernel:specifier-type '(integer -10 (6)))
       (sb-kernel:specifier-type '(single-float -8.0 4.0)))
      (sb-kernel:specifier-type '(single-float -8.0 5.0))))
