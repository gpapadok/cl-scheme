(in-package #:cl-scheme/test)

(deftest set-test
  (ok-eval (let ((seq '(1 2 3)))
	     (and (set-car! seq 0)
		  (equal? seq '(0 2 3))
		  (set-cdr! seq '(4 5))
		  (equal? seq '(0 4 5))))))

(deftest do-test
  (ok-eval (let ((x 1))
	         (= (do ((i 1 (+ i 1)))
		            ((> i 10) x)
		          (set! x (+ i x)))
		        56)))
  (ok-eval (equal?
	        (do ((vec (make-vector 5))
		         (i 0 (+ i 1)))
		        ((= i 5) vec)
	          ;; (print vec)
	          (vector-set! vec i i))
	        #(0 1 2 3 4))))

(deftest delay-test
  (ok-eval (= 3 (force (delay (+ 1 2)))))
  (ok-eval (equal? '(3 3)
		           (let ((p (delay (+ 1 2))))
		             (list (force p) (force p)))))
  (ok-eval (equal? (list 3 3) (let ((p (delay (+ 1 2))))
				                (list (force p) (force p)))))
  (ok-eval (let* ((counter 0)
		          (promise (delay (+ 1 counter))))
	         (= (force promise) (force promise) (force promise) 1))))

(deftest case-test
  (ok-eval (equal? 'composite
		           (case (* 2 3)
		             ((2 3 5 7) 'prime)
		             ((1 4 6 8 9) 'composite))))
  (ok-eval (null? (case (car '(c d))
		            ((a) 'a)
		            ((b) 'b))))
  (ok-eval (equal? 'consonant
		           (case (car '(c d))
		             ((a e i o u) 'vowel)
		             ((w y) 'semivowel)
		             (else 'consonant)))))

(deftest quasiquote-test
  (ok-eval (equal? '(+ 1 2) `(+ 1 (unquote (- 3 1)))))
  (ok-eval (equal? '(1 2 3 4 5) `(1
				                  (unquote-splicing
				                   (map (lambda (x) (+ 1 x))'(1 2 3)))
				                  5))))

(deftest macro-test
  (ok-eval (begin
	        (define-macro (inc x) `(+ 1 (unquote x)))
	        (= 2 (inc 1)))))

(deftest special-form-test
  (ok-eval (= 20 (begin
 		          (define x 1)
 		          (set! x 2)
 		          (* x 10))))
  (ok-eval (= 1 (if #t 1 2)))
  (ok-eval (= 2 (if #f 1 2)))
  (ok-eval (= 15 (if (< 10 3) (+ 1 2) (* 3 5))))
  (ok-eval (= 1 (or 1 2 #f)))
  (ok-eval (equal? #f (or #f #f #f)))
  (ok-eval (= 3 (and 1 2 3)))
  (ok-eval (equal? #f (and 1 #f 3)))
  (ok-eval (begin (define x 1) (= 1 x)))
  (ok-eval (begin (define name "mpampis") (string=? "mpampis" name))))

(deftest letrec-test
  (ok-eval (= 3628800
	          (letrec
	           ((fac (lambda (n)
		               (if (<= n 1)
			               n
			               (* n (fac (- n 1)))))))
	           (fac 10))))
  (ok-eval (letrec ((f (lambda (n)
			             (if (= n 0)
			                 n
			                 (f (- n 1))))))
		           (f 10))))

(deftest let-test
  (testing "let form"
    (ok-eval (equal? '(+ 1 2) (let ((x 1)) `(+ (unquote x) 2))))
    (ok-eval (= 3 (let ((x 1) (y 2)) (+ x y))))
    (ok-eval (begin
	          (define x 1)
	          (let ((y (+ 10 x)))
	            (= 11 y))))
    (ok-eval (= 35
	            (let ((x 2) (y 3))
		          (let ((x 7)
		                (z (+ x y)))
		            (* z x)))))
    (ok-eval (= 2
	            (let ((x 1))
		          (let ((x 10)
		                (y (+ x 1)))
		            y))))
    (ok-eval (= 70
	            (let ((x 2) (y 3))
		          (let* ((x 7)
		                 (z (+ x y)))
		            (* z x)))))
    (ok-eval (equal? '((6 1 3) (-5 -2))
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
    (ok-eval (let ((x 1)                ; This should not work
		           (x 2))
	           x))
    (ok-eval (let ((even?
		             (lambda (n)
		               (if (zero? n)
			               #t
			               (odd? (- n 1)))))
		           (odd?
		             (lambda (n)
		               (if (zero? n)
			               #f
			               (even? (- n 1))))))
	           (even? 88)))))
