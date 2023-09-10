(ql:quickload :fiveam)
(use-package :fiveam)

(load "scheme.lisp")

(def-suite scheme-tests)
(in-suite scheme-tests)

(setf *run-test-when-defined* t)

(defmacro is-eval (sexp)
  `(is (evaluate ',sexp)))

(test numeric-operations-test
  (is-eval (= 2 (+ 1 1)))
  (is-eval (= 15  (+ 2 (* 3 3) 4)))
  (is-eval (= 0 (- 10 6 4)))
  (is-eval (= 5 (/ 15 3)))
  (is-eval (= 1 (abs -1)))
  (is-eval (>= 3 2 1))
  (is-eval (< 3 (max 1 2 4)))
  )

(test list-test
  (is-eval (equal? '(1 2 3) (quote (1 2 3))))
  (is-eval (equal? '(1 2 3) (list 1 2 3)))
  )

(test procedures-test
  (is-eval (= 10 ((lambda (x) (* 5 x)) 2)))
  (is-eval (begin
	    (define (fac n)
		(if (= 1 n)
		    1
		    (* n (fac (- n 1)))))
	    (= 3628800 (fac 10))))
  (is-eval (begin
	    (define (A x y)
		(cond ((= 0 x) (+ y 1))
		      ((= 0 y) (A (- x 1) 1))
		      (#t (A (- x 1) (A x (- y 1))))))
	    (= 7 (A 2 2))))
  (is-eval (equal? '(1 2 3) (cons 1 '(2 3))))
  (is-eval (= 1 (car '(1 2 3))))
  (is-eval (equal? '(4 5 6) (list 4 5 6)))
  (is-eval (= 3 (length '(1 2 3))))
  )

(test let-test
  (is-eval (equal? '(+ 1 2) (let ((x 1)) `(+ (unquote x) 2))))
  (is-eval (= 3 (let ((x 1) (y 2)) (+ x y))))
  (is-eval (begin
	    (define x 1)
	    (let ((y (+ 10 x)))
	      (= 11 y))))
  (is-eval (= 35
	      (let ((x 2) (y 3))
		(let ((x 7)
		      (z (+ x y)))
		  (* z x)))))
  (is-eval (= 2
	      (let ((x 1))
		(let ((x 10)
		      (y (+ x 1)))
		  y))))
  (is-eval (= 70
	      (let ((x 2) (y 3))
		(let* ((x 7)
		       (z (+ x y)))
		  (* z x)))))
  )

(test special-form-test
  (is-eval (= 20 (begin
 		  (define x 1)
 		  (set! x 2)
 		  (* x 10))))
  (is-eval (= 1 (if #t 1 2)))
  (is-eval (= 2 (if #f 1 2)))
  (is-eval (= 15 (if (< 10 3) (+ 1 2) (* 3 5))))
  (is-eval (= 1 (or 1 2 #f)))
  (is-eval (equal? #f (or #f #f #f)))
  (is-eval (= 3 (and 1 2 3)))
  (is-eval (equal? #f (and 1 #f 3)))
  (is-eval (begin (define x 1) (= 1 x)))
  (is-eval (begin (define name "mpampis") (string=? "mpampis" name)))
  )

(test closure-test
  (is-eval (begin
	    (define (adder x)
		(lambda (y) (+ x y)))
	    (= 12 ((adder 10) 2))))
  (is-eval (begin
	    (define add4
	      (let ((x 4))
		(lambda (y) (+ x y))))
	    (= 6 (add4 2))))
  (is-eval (= 12 ((let ((x 2))
		    (lambda (y) (* x y))) 6)))
  )

(test scope-test
  (is-eval (begin
	    (define x 1)
	    (define (add2 y)
		(define x 2)
	      (+ x y))
	    (and (= 1 x) (= 8 (add2 6)))))
  (is-eval (begin
	    (define x 10)
	    (define k
		(let ((x 1)
		      (y 2))
		  (+ x y)))
	    (and (= 10 x) (= 3 k))))
  )

(test quasiquote-test
  (is-eval (equal? '(+ 1 2) `(+ 1 (unquote (- 3 1)))))
  (is-eval (equal? '(1 2 3 4 5) `(1
				  (unquote-splicing
				   (map (lambda (x) (+ 1 x))'(1 2 3)))
				  5)))
  )

(test macro-test
  (is-eval (begin
	    (define-macro (inc x) `(+ 1 (unquote x)))
	    (= 2 (inc 1))))
  )

(test higher-order-functions-test
  (is-eval (equal? '(1 2 3) (map (lambda (x) (+ 1 x)) '(0 1 2))))
  (is-eval (equal? '(2 4 6) (filter even? '(1 2 3 4 5 6 7))))
  (is-eval (= 21 (reduce + 0 '(1 2 3 4 5 6))))
  )

(test case-test
  (is-eval (equal? 'composite
		   (case (* 2 3)
		     ((2 3 5 7) 'prime)
		     ((1 4 6 8 9) 'composite))))
  (is-eval (null? (case (car '(c d))
		   ((a) 'a)
		   ((b) 'b))))
  (is-eval (equal? 'consonant
		   (case (car '(c d))
		     ((a e i o u) 'vowel)
		     ((w y) 'semivowel)
		     (else 'consonant))))
  )
