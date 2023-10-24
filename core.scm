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

(define (memq obj lst)
  (and (pair? lst)
       (if (eq? obj (car lst))
	   lst
	   (memq obj (cdr lst)))))

(define (memv obj lst)
  (and (pair? lst)
       (if (eqv? obj (car lst))
	   lst
	   (memv obj (cdr lst)))))

(define (member obj lst)
  (and (pair? lst)
       (if (equal? obj (car lst))
	   lst
	   (member obj (cdr lst)))))
