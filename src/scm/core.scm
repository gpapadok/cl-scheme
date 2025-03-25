;; TODO: Implement iterative constructs like do, for, while, until

(define (map op seq) ; TODO: This should work for multiple sequences
  (if (null? seq)
      seq
      (cons (op (car seq)) (map op (cdr seq)))))

(define (filter pred seq)
  (if (null? seq)
      seq
      (if (pred (car seq))
          (cons (car seq) (filter pred (cdr seq)))
          (filter pred (cdr seq)))))

(define (reduce op init-value seq)
  (if (null? seq)
      init-value
      (reduce op (op init-value (car seq)) (cdr seq))))

(define (for-each proc seq)
  (if (not (null? seq))
      (begin
	(proc (car seq))
	(for-each proc (cdr seq)))))

;;;

(define (%member obj lst compare)
  (and (pair? lst)
       (if (compare obj (car lst))
	   lst
	   (%member obj (cdr lst) compare))))

(define (memq obj lst)
  (%member obj lst eq?))

(define (memv obj lst)
  (%member obj lst eqv?))

(define (member obj lst)
  (%member obj lst equal?))

(define (%assoc key lst compare)
  (let ((pair (car lst)))
    (and (pair? pair)
	 (if (compare key (car pair))
	     pair
	     (%assoc key (cdr lst) compare)))))

(define (assq key lst)
  (%assoc key lst eq?))

(define (assv key lst)
  (%assoc key lst eqv?))

(define (assoc key lst)
  (%assoc key lst equal?))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (zero? n)
  (= n 0))
