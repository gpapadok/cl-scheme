(ql:quickload :fiveam)
(use-package :fiveam)

(load "scheme.lisp")

(def-suite scheme-tests)
(in-suite scheme-tests)

(setf *run-test-when-defined* t)

(defmacro is-eval (sexp)
  `(is (evaluate ',sexp)))

(defmacro deftest (name &body body)
  `(test ,name
     (let ((*global-env* (copy-list *global-env*)))
       ,@body)))

(deftest numeric-operations-test
  (is-eval (= 2 (+ 1 1)))
  (is-eval (= 15  (+ 2 (* 3 3) 4)))
  (is-eval (= 0 (- 10 6 4)))
  (is-eval (= 5 (/ 15 3)))
  (is-eval (= 1 (abs -1)))
  (is-eval (>= 3 2 1))
  (is-eval (< 3 (max 1 2 4)))
  (is-eval (> 4 (min 1 6 7)))
  )

(deftest list-test
  (is-eval (equal? '(1 2 3) (quote (1 2 3))))
  (is-eval (equal? '(1 2 3) (list 1 2 3)))
  )

(deftest procedures-test
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
  (is-eval (procedure? car))
  (is-eval (= 3 (apply + (list 1 2))))
  )

(deftest let-test
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
  (is-eval (equal? '((6 1 3) (-5 -2))
		   (let loop ((numbers '(3 -2 1 6 -5))
			      (nonneg '())
			      (neg '()))
		     (cond ((null? numbers) (list nonneg neg))
			   ((>= (car numbers) 0)
			    (loop (cdr numbers)
				  (cons (car numbers) nonneg)
				  neg))
			   ((< (car numbers) 0)
			    (loop (cdr numbers)
				  nonneg
				  (cons (car numbers) neg)))))))
  (is-eval (let ((x 1)			; This should not work
		 (x 2))
	     x))
  (is-eval (let ((even?
		   (lambda (n)
		     (if (zero? n)
			 #t
			 (odd? (- n 1)))))
		 (odd?
		   (lambda (n)
		     (if (zero? n)
			 #f
			 (even? (- n 1))))))
	     (even? 88)))
  )

(deftest letrec-test
  (is-eval (= 3628800
	      (letrec
	       ((fac (lambda (n)
		       (if (<= n 1)
			   n
			   (* n (fac (- n 1)))))))
	       (fac 10))))
  (is-eval (letrec ((f (lambda (n)
			 (if (= n 0)
			     n
			     (f (- n 1))))))
		   (f 10)))
  )

(deftest special-form-test
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

(deftest closure-test
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

(deftest scope-test
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

(deftest quasiquote-test
  (is-eval (equal? '(+ 1 2) `(+ 1 (unquote (- 3 1)))))
  (is-eval (equal? '(1 2 3 4 5) `(1
				  (unquote-splicing
				   (map (lambda (x) (+ 1 x))'(1 2 3)))
				  5)))
  )

(deftest macro-test
  (is-eval (begin
	    (define-macro (inc x) `(+ 1 (unquote x)))
	    (= 2 (inc 1))))
  )

(deftest higher-order-functions-test
  (is-eval (equal? '(1 2 3) (map (lambda (x) (+ 1 x)) '(0 1 2))))
  (is-eval (equal? '(2 4 6) (filter even? '(1 2 3 4 5 6 7))))
  (is-eval (= 21 (reduce + 0 '(1 2 3 4 5 6))))
  (is-eval (equal? '(b e h)
		   (map cadr '((a b) (d e) (g h)))))
  (is-eval (equal? (list 1 4 27 256 3125)
		   (map (lambda (x) (expt x x))
			(list 1 2 3 4 5))))
  ;; TODO: Implement `&rest` in lambda list
  ;; (is-eval (equal? (list 5 7 9)
  ;; 		   (map + '(1 2 3) '(4 5 6))))
  (is-eval (equal? '(1 2)
		   (let ((count 0))
		     (map (lambda (ignored)
			    (set! count (+ count 1))
			    count)
			  '(a b)))))
  (is-eval (equal? #(0 1 4 9 16)
		   (let ((v (make-vector 5)))
		     (for-each (lambda (i)
				 (vector-set! v i (* i i)))
			       '(0 1 2 3 4))
		     v)))
  )

(deftest case-test
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

(deftest vector-test
  (is-eval (let ((v (make-vector 4)))
	     (and (vector? v)
		  (equal? #(0 0 0 0) v))))
  (is-eval (begin
	    (let ((v (make-vector 3 2)))
	      (and (equal? #(2 2 2) v)
		   (vector-set! v 1 3)
		   (equal? #(2 3 2) v)))))
  )

(deftest set-test
  (is-eval (let ((seq '(1 2 3)))
	     (and (set-car! seq 0)
		  (equal? seq '(0 2 3))
		  (set-cdr! seq '(4 5))
		  (equal? seq '(0 4 5)))))
      )

(deftest do-test
  (is-eval (let ((x 1))
	     (= (do ((i 1 (+ i 1)))
		    ((> i 10) x)
		  (set! x (+ i x)))
		56)))
  (is-eval (equal?
	    (do ((vec (make-vector 5))
		 (i 0 (+ i 1)))
		((= i 5) vec)
	      ;; (print vec)
	      (vector-set! vec i i))
	    #(0 1 2 3 4)))
  )

(deftest delay-test
  (is-eval (= 3 (force (delay (+ 1 2)))))
  (is-eval (equal? '(3 3)
		   (let ((p (delay (+ 1 2))))
		     (list (force p) (force p)))))
  (is-eval (equal? (list 3 3) (let ((p (delay (+ 1 2))))
				(list (force p) (force p)))))
  (is-eval (let* ((counter 0)
		  (promise (delay (+ 1 counter))))
	     (= (force promise) (force promise) (force promise) 1)))
  )
