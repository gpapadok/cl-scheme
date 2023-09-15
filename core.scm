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
