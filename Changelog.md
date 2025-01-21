# Changelog

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
