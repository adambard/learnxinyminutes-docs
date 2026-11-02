---
name: Lispex
contributors:
    - ["STUDIO HAZE", "https://www.lispex.com/"]
filename: learnlispex.lspx
---

Lispex is a language for writing business rules, such as a publisher's house
style. It cannot touch files or the network, so it is safe to run inside any
program.

It follows the Scheme tradition, but is its own language. Do not assume that
every Scheme extension or library is available.

Save this code as `learnlispex.lspx` and run `lispex learnlispex.lspx`.
The examples use the official Lispex 1.21.1 CLI. It displays successful
top-level values; definitions do not produce a displayed value.

```text
; A semicolon starts a line comment.
#| Block comments can be nested: #| inner comment |# outer comment. |#

; A list in procedure position calls its first element with the rest.
(+ 2 3)                              ; => 5

; Quotation produces data instead of calling a procedure.
'(+ 2 3)                             ; => (+ 2 3)

; Bindings do not require static type annotations.
(define language "Lispex")
(list 'hello language)               ; => (hello "Lispex")
(list (string? language) (number? 3)) ; => (#t #t)

; Exact integers are arbitrary precision; fractions stay exact.
(+ 99999999999999999999 1)            ; => 100000000000000000000
(/ 1 3)                              ; => 1/3
(+ 1/3 1/6)                          ; => 1/2

; A decimal or exponent makes an inexact, finite 64-bit real.
(+ 1 2.0)                            ; => 3.0
(list (= 2 2.0) (= 1/10 0.1))        ; => (#t #f)
; Numeric comparison does not round an exact operand to a float first.
; Complex numbers and infinity/NaN are not part of this numeric model.

; A procedure can be defined by name or created with lambda.
(define (double n) (* n 2))
(map (lambda (n) (+ 1 (double n))) '(1 2 3)) ; => (3 5 7)

; Only #f is false: zero and the empty list are true.
(if 0 'yes 'no)                      ; => yes
(if #f 'yes 'no)                     ; => no

; let introduces local bindings; let* makes them sequential.
(let ((x 10) (y 20)) (+ x y))         ; => 30
(let* ((x 10) (y (+ x 2))) y)         ; => 12

; A named let expresses a loop using tail calls.
(let loop ((n 5) (total 0))
  (if (= n 0)
      total
      (loop (- n 1) (+ total n))))    ; => 15

; Definitions are shared cells; set! changes an existing binding.
(define counter 0)
(define (next)
  (set! counter (+ counter 1))
  counter)
(list (next) (next))                 ; => (1 2)

; Lists, pairs, vectors, and bytevectors are different values.
(list (car '(a b)) (cdr '(a b)))      ; => (a (b))
(cons 'a 'b)                         ; => (a . b)
(vector-ref #(10 20 30) 1)           ; => 20
(bytevector-u8-ref #u8(65 66) 0)      ; => 65

; Prefixes expand into quote/quasiquote/unquote forms.
(let ((x 2)) `(one ,x ,@'(3 4)))      ; => (one 2 3 4)

; Procedures can return multiple values.
(call-with-values (lambda () (values 2 3)) +) ; => 5

; Catch runtime errors with guard and inspect the error object.
(guard (problem
         ((error-object? problem) (error-object-message problem)))
  (error "bad input"))               ; => "bad input"

; Reading and static errors happen before guard can run. For example,
; an unclosed parenthesis must be repaired in the source.
; Resource termination is also distinct from a catchable runtime error.

; Datum comments (#;) and #lang directives are not accepted.
; Ordinary source execution is for trusted programs. Use the separate
; bounded evaluator interface for an application's explicit work and
; memory limits; the caller controls files, networking, and other actions.
```

## Further reading

- [First program](https://www.lispex.com/v1.21/en/docs/getting-started)
- [Numbers](https://www.lispex.com/v1.21/en/docs/manual/numbers)
- [Errors](https://www.lispex.com/v1.21/en/docs/guides/errors)
- [Application integration][integration]

[integration]: https://www.lispex.com/v1.21/en/docs/guides/choosing-runtime
