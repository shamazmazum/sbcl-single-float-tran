# sbcl-single-float-tran

This system tells SBCL how to use math functions from libm for single-float
type. In other words rather than converting single-float to double-float,
calling `sin` and converting back, SBCL will simply call `sinf`. Also it makes
use of some SSE instructions like `min(max)ss(d)` or `sqrtss`.

Here is an example. Consider this code:

``` lisp
(defun foo (x)
  (declare (optimize (speed 3))
           (type single-float x))
  (sinh (1+ x)))
```

This is the produced assembly before loading `sbcl-single-float-tran`:

``` lisp
CL-USER> (disassemble 'foo)
; disassembly for FOO
; Size: 65 bytes. Origin: #x226390EF                          ; FOO
; 0EF:       F30F580DC1FFFFFF ADDSS XMM1, [RIP-63]            ; [#x226390B8]
; 0F7:       F30F5AC9         CVTSS2SD XMM1, XMM1
; 0FB:       488BDC           MOV RBX, RSP
; 0FE:       4883E4F0         AND RSP, -16
; 102:       660F28C1         MOVAPD XMM0, XMM1
; 106:       B801000000       MOV EAX, 1
; 10B:       FF142558040020   CALL QWORD PTR [#x20000458]     ; sinh
; 112:       488BE3           MOV RSP, RBX
; 115:       F20F5AC0         CVTSD2SS XMM0, XMM0
; 119:       0FC6C0FC         SHUFPS XMM0, XMM0, #4r3330
; 11D:       660F7EC2         MOVD EDX, XMM0
; 121:       48C1E220         SHL RDX, 32
; 125:       80CA19           OR DL, 25
; 128:       488BE5           MOV RSP, RBP
; 12B:       F8               CLC
; 12C:       5D               POP RBP
; 12D:       C3               RET
; 12E:       CC10             INT3 16                         ; Invalid argument count trap
NIL
```

and this is after loading `sbcl-single-float-tran` and recompilation:

``` lisp
CL-USER> (disassemble 'foo)
; disassembly for FOO
; Size: 46 bytes. Origin: #x2263B0BF                          ; FOO
; BF:       F30F580DC1FFFFFF ADDSS XMM1, [RIP-63]             ; [#x2263B088]
; C7:       4883E4F0         AND RSP, -16
; CB:       0F28C1           MOVAPS XMM0, XMM1
; CE:       B801000000       MOV EAX, 1
; D3:       FF1425C8140020   CALL QWORD PTR [#x200014C8]      ; sinhf
; DA:       660F7EC2         MOVD EDX, XMM0
; DE:       48C1E220         SHL RDX, 32
; E2:       80CA19           OR DL, 25
; E5:       488BE5           MOV RSP, RBP
; E8:       F8               CLC
; E9:       5D               POP RBP
; EA:       C3               RET
; EB:       CC10             INT3 16                          ; Invalid argument count trap
NIL
```

Note, that `sbcl-single-float-tran` correctly handles domain of a function:

``` lisp
(defun foo (x)
  (declare (optimize (speed 3))
           (type (single-float -1f0) x))
  (+ (log x) (log (1+ x))))
```

``` lisp
CL-USER> (disassemble 'foo)
; disassembly for FOO
; Size: 109 bytes. Origin: #x2263B1A0                         ; FOO
; 1A0:       488945F8         MOV [RBP-8], RAX
; 1A4:       4883EC10         SUB RSP, 16
; 1A8:       488BD0           MOV RDX, RAX
; 1AB:       B902000000       MOV ECX, 2
; 1B0:       48892C24         MOV [RSP], RBP
; 1B4:       488BEC           MOV RBP, RSP
; 1B7:       B8021E3720       MOV EAX, #x20371E02             ; #<FDEFN LOG>
; 1BC:       FFD0             CALL RAX
; 1BE:       488B45F8         MOV RAX, [RBP-8]
; 1C2:       4C8BF2           MOV R14, RDX
; 1C5:       66480F6EC8       MOVQ XMM1, RAX
; 1CA:       0FC6C9FD         SHUFPS XMM1, XMM1, #4r3331
; 1CE:       F30F580D86FFFFFF ADDSS XMM1, [RIP-122]           ; [#x2263B15C]
; 1D6:       488BDC           MOV RBX, RSP
; 1D9:       4883E4F0         AND RSP, -16
; 1DD:       0F28C1           MOVAPS XMM0, XMM1
; 1E0:       B801000000       MOV EAX, 1
; 1E5:       FF142588140020   CALL QWORD PTR [#x20001488]     ; logf
; 1EC:       488BE3           MOV RSP, RBX
; 1EF:       660F7EC7         MOVD EDI, XMM0
; 1F3:       48C1E720         SHL RDI, 32
; 1F7:       4080CF19         OR DIL, 25
; 1FB:       498BD6           MOV RDX, R14
; 1FE:       FF1425E800A021   CALL QWORD PTR [#x21A000E8]     ; GENERIC-+
; 205:       488BE5           MOV RSP, RBP
; 208:       F8               CLC
; 209:       5D               POP RBP
; 20A:       C3               RET
; 20B:       CC10             INT3 16                         ; Invalid argument count trap
NIL
```

## MIN and MAX

`sbcl-single-float-tran` converts calls to `min` and `max` to SSE instructions
where possible. Despite the name of this library, this works for double float
numbers too. The result of such call is converted to the largest format of
floating point arguments if one of the arguments is a floating point value or
otherwise remains as is. E.g.

~~~~
(MIN SINGLE-FLOAT DOUBLE-FLOAT FIXNUM) -> DOUBLE-FLOAT
(MIN FIXNUM RATIO) -> FIXNUM or RATIO
~~~~

### Discrepancy with normal function call

This introduce a discrepancy with a normal call to MIN:

~~~
CL-USER> (funcall (lambda (x) (declare (type single-float x)) (min 1 x)) 3.0)
1.0
CL-USER> (min 1 3.0)
1
~~~

According to CLHS, MIN can return either `1` or `1.0` here.

## Portability

This system can be loaded on any other implementation but obviously has an
effect on SBCL only.

## TODO

Write tests.
