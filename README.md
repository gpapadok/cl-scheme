# Scheme in Common Lisp

A Scheme written in Common Lisp to take advantage of the built-in tools Lisp offers to read and evaluate structured expressions.
This implementation is a subset of [r5rs.pdf](https://conservatory.scheme.org/schemers/Documents/Standards/R5RS/r5rs.pdf). Tested and developed only with Steel Bank Common Lisp.

Run with:
```
sbcl --script scheme.lisp
```

## Examples

##### Basic

```
> (+ 4 5)
9
> (cons 1 (list 2 3 4))
(1 2 3 4)
```

##### Recursion

```
> (define (fac n) (if (= n 1) 1 (* n (fac (- n 1)))))
FAC
> (fac 10)
3628800
```
##### Higher order functions

```
> (define (inc x) (+ x 1))
INC
> (reduce + 0 (filter even? (map inc '(0 1 2 3 4 5))))
12
```
