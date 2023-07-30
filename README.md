# Scheme in Common Lisp

A Scheme written in Common Lisp to take advantage of the built-in tools Lisp offers to read and evaluate structured expressions.
Not a full implementation (yet). Tested and developed only with Steel Bank Common Lisp.

Some cool things you can do:
```
sbcl --script scheme.lisp
> (+ 4 5)
9
> (cons 1 (list 2 3 4))
(1 2 3 4)
> (define (fac n) (if (= n 1) 1 (* n (fac (- n 1)))))
FAC
> (fac 10)
3628800
```
