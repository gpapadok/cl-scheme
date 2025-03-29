(in-package #:cl-scheme/test)

;; TODO: Add tests that should fail

(deftest numeric-operations-test
  (testing "simple numeric operations"
    (ok-eval (= 2 (+ 1 1)))
    (ok-eval (= 15 (+ 2 (* 3 3) 4)))
    (ok-eval (= 0 (- 10 6 4)))
    (ok-eval (= 5 (/ 15 3)))
    (ok-eval (= 1 (abs -1)))
    (ok-eval (>= 3 2 1))
    (ok-eval (< 3 (max 1 2 4)))
    (ok-eval (> 4 (min 1 6 7)))))

(deftest equality-test
  (testing "eq?"
    (ok-eval (eq? 'a 'a))
    (ok-eval (not (eq? (list 'a) (list 'a))))
    (ok-eval (eq? '() '()))
    (ok-eval (eq? car car))
    (ok-eval (let ((n (+ 2 3)))
               (eq? n n)))
    (ok-eval (let ((x '(a)))
               (eq? x x)))
    (ok-eval (let ((x '#()))
               (eq? x x)))
    (ok-eval (let ((x (lambda (x) x)))
               (eq? x x))))
  (testing "eqv?"
    (ok-eval (and (eqv? #t #t) (eqv? #f #f) (not (eqv? #t #f))))
    (ok-eval (and (eqv? 'a 'a) (not (eqv? 'a 'b))))
    (ok-eval (eqv? 'foo 'foo))
    ;; (ok-eval (and (eqv? 1 1) (not (eqv? 1 1.0))))
    (ok-eval (eqv? #\a #\a))
    (ok-eval (eqv? '() '()))
    (ok-eval (and (eqv? 2 2) (not (eqv? 2 2.0))))
    (ok-eval (not (eqv? (cons 1 2) (cons 1 2))))
    (ok-eval (not (eqv? (lambda () 1)
                        (lambda () 2))))
    (ok-eval (eqv? #f 'nil))
    (ok-eval (let ((p (lambda (x) x)))
               (eqv? p p)))
    (ok-eval (let* ((l (list 1 2 3))
                    (k l))
               (and (eqv? l l)
                    (eqv? l k)
                    (not (eqv? l (list 1 2 3))))))
    (ok-eval (begin
              (define gen-counter
                  (lambda ()
                    (let ((n 0 ))
                      (lambda () (set! n (+ n 1)) n))))
              (let ((g (gen-counter)))
                (and (eqv? g g)
                     (not (eqv? (gen-counter)
                                (gen-counter))))))))
  (testing "equal?"
    (ok-eval (equal? 'a 'a))
    (ok-eval (equal? '(a) '(a)))
    (ok-eval (equal? '(a (b) c)
                     '(a (b) c)))
    (ok-eval (equal? "abc" "abc"))
    (ok-eval (equal? 2 2))
    (ok-eval (equal? (make-vector 5 'a)
                     (make-vector 5 'a)))
    (ok-eval (not (equal? (lambda (x) x)
                          (lambda (y) y))))))

(deftest list-test
  (testing "generic list functions"
    (ok-eval (equal? '(1 2 3) (quote (1 2 3))))
    (ok-eval (equal? '(1 2 3) (list 1 2 3)))
    (ok-eval (equal? '(3 4) (list-tail '(1 2 3 4) 2)))
    (ok-eval (equal? 'c (list-ref '(a b c d) 2))))
  (testing "pair?"
    (ok-eval (pair? '(a . b)))
    (ok-eval (pair? '(a b c)))
    (ok-eval (not (pair? '())))
    (ok-eval (not (pair? '#(a b)))))
  (testing "memq"
    (ok-eval (equal? '(a b c) (memq 'a '(a b c))))
    (ok-eval (equal? '(b c) (memq 'b '(a b c))))
    (ok-eval (not (memq 'a '(b c d))))
    (ok-eval (equal? '(101 102) (memq 101 '(100 101 102)))))
  (testing "memv"
    (ok-eval (equal? '(101 102) (memv 101 '(100 101 102)))))
  (testing "member"
    (ok-eval (equal? '((a) c) (member (list 'a) '(b (a) c))))))

(deftest alist-test
  (testing "alist"
    (ok-eval (begin
	          (define e '((a 1) (b 2) (c 3)))
	          (and (equal? '(a 1) (assq 'a e))
		           (equal? '(b 2) (assq 'b e))
		           (not (assq 'd e)))))
    (ok-eval (not (assq (list 'a) '(((a) ((b)) ((c)))))))
    (ok-eval (equal? '(5 7) (assq 5 '((2 3) (5 7) (11 13)))))
    (ok-eval (equal? '(5 7) (assv 5 '((2 3) (5 7) (11 13)))))
    (ok-eval (equal? '((a)) (assoc (list 'a) '(((a)) ((b)) ((c))))))))

(deftest procedures-test
  (testing "procedures"
    (ok-eval (= 10 ((lambda (x) (* 5 x)) 2)))
    (ok-eval (begin
	          (define (fac n)
		          (if (= 1 n)
		              1
		              (* n (fac (- n 1)))))
	          (= 3628800 (fac 10))))
    (ok-eval (begin
	          (define (A x y)
		          (cond ((= 0 x) (+ y 1))
		                ((= 0 y) (A (- x 1) 1))
		                (#t (A (- x 1) (A x (- y 1))))))
	          (= 7 (A 2 2))))
    (ok-eval (equal? '(1 2 3) (cons 1 '(2 3))))
    (ok-eval (= 1 (car '(1 2 3))))
    (ok-eval (equal? '(4 5 6) (list 4 5 6)))
    (ok-eval (= 3 (length '(1 2 3))))
    (ok-eval (procedure? car))
    (ok-eval (= 3 (apply + (list 1 2))))))

(deftest closure-test
  (ok-eval (begin
	        (define (adder x)
		        (lambda (y) (+ x y)))
	        (= 12 ((adder 10) 2))))
  (ok-eval (begin
	        (define add4
	            (let ((x 4))
		          (lambda (y) (+ x y))))
	        (= 6 (add4 2))))
  (ok-eval (= 12 ((let ((x 2))
		            (lambda (y) (* x y))) 6))))

(deftest scope-test
  (ok-eval (begin
	        (define x 1)
	        (define (add2 y)
		        (define x 2)
	          (+ x y))
	        (and (= 1 x) (= 8 (add2 6)))))
  (ok-eval (begin
	        (define x 10)
	        (define k
		        (let ((x 1)
		              (y 2))
		          (+ x y)))
	        (and (= 10 x) (= 3 k)))))

(deftest higher-order-functions-test
  (ok-eval (equal? '(1 2 3) (map (lambda (x) (+ 1 x)) '(0 1 2))))
  (ok-eval (equal? '(2 4 6) (filter even? '(1 2 3 4 5 6 7))))
  (ok-eval (= 21 (reduce + 0 '(1 2 3 4 5 6))))
  (ok-eval (equal? '(b e h)
		           (map cadr '((a b) (d e) (g h)))))
  (ok-eval (equal? (list 1 4 27 256 3125)
		           (map (lambda (x) (expt x x))
			            (list 1 2 3 4 5))))
  ;; TODO: Implement `&rest` in lambda list
  ;; (ok-eval (equal? (list 5 7 9)
  ;; 		   (map + '(1 2 3) '(4 5 6))))
  (ok-eval (equal? '(1 2)
		           (let ((count 0))
		             (map (lambda (ignored)
			                (set! count (+ count 1))
			                count)
			              '(a b)))))
  (ok-eval (equal? #(0 1 4 9 16)
		           (let ((v (make-vector 5)))
		             (for-each (lambda (i)
				                 (vector-set! v i (* i i)))
			                   '(0 1 2 3 4))
		             v))))

(deftest vector-test
  (ok-eval (let ((v (make-vector 4)))
	     (and (vector? v)
		  (equal? #(0 0 0 0) v))))
  (ok-eval (begin
	    (let ((v (make-vector 3 2)))
	      (and (equal? #(2 2 2) v)
		   (vector-set! v 1 3)
		   (equal? #(2 3 2) v))))))
