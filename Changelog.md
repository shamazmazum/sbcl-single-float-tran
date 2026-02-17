# Changelog

## Version 1.0

* Add result constrains for MIN/MAX so that SBCL can detect unreachable code in
  the following:

  ```lisp
  (defun foo (x y)
  (unless (<= (min x y) x)
    (error "never happens")))
  ```

  and

  ```lisp
  (defun bar (x y)
  (unless (<= x (max x y))
    (error "never happens")))
  ```

## Version 0.6

* Remove a transform for `sqrt` with a single float argument: SBCL can do this
  out of the box for some time.
* This system can now be compiled to a `.fasl` file with GNU make.

## Version 0.5

* Add transforms for `asin`, `acos` and `atan`.
* Better type inference in some cases.

## Version 0.4

* Improvement: Result type inference for MIN and MAX. For example, now a call to
  `sqrt` in something like `(sqrt (max (abs x) (abs y)))` is compiled to
  `sqrtsd` CPU instruction for double float numbers on x86-64. The type of this
  expression is known to be `(double-float 0d0)`.
* Incompatible change: drop support for SBCL version < 2.2.1
* Bug fix: Constant folding for MIN and MAX produces the same result as an
  equivalent compiled code.

## Version 0.3.1

Bugfix release

* Define TWO-ARG-(MIN/MAX) as normal functions too. Required for constant
  folding.

## Version 0.3

Further improvements

* MIN/MAX are transformed to SSE instructions on x86-64 when appropriate

## Version 0.2

Some improvements

* Add a transform for EXPT when appropriate

## Version 0.1

Initial commit

* Add transforms for EXP, SIN, COS, TAH, SINH, COSH, TANH to single float C
  counterparts (expf, sinf etc.) when appropriate.
