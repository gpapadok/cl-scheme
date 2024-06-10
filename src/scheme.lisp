(in-package #:cl-scheme)

;; Use cl built-ins for booleans
(eval-when (:compile-toplevel)
  (set-dispatch-macro-character #\# #\t #'(lambda (stream subchar arg)
					    (declare (ignore stream
							     subchar
							     arg))
					    t))
  (set-dispatch-macro-character #\# #\f #'(lambda (stream subchar arg)
					    (declare (ignore stream
							     subchar
							     arg))
					    nil)))

(defvar *global-env* nil
  "Interpreter's global environment.")

(defun procedurep (item)
  (and (consp item) (eq 'procedure (car item))))

(defun macrop (item)
  (and (consp item) (eq 'macro (car item))))

(defun special-form-p (expr)
  (and (consp expr)
       (member (car expr) (mapcar #'car *special-forms*))))

(defun evaluate-special-form (expr env)
  (if-let (eval-op (lookup (car expr) *special-forms*))
    (funcall eval-op (cdr expr) env)
    (error "Form ~a not implemented~%" (car expr))))

(defun self-evaluating-p (expr)
  (or (numberp expr) (stringp expr) (keywordp expr) (vectorp expr)))

(defun evaluate (expr &optional (env *global-env*))
  (cond ((self-evaluating-p expr) expr)
	((symbolp expr) (lookup expr env))
	((special-form-p expr) (evaluate-special-form expr env))
	((consp expr)
	 (let ((operator (evaluate (car expr) env)))
	   (cond
	     ((functionp operator)
	      (apply operator
		     (mapcar #'(lambda (form)
				 (funcall #'evaluate form env))
			     (cdr expr))))
	     ((or (procedurep operator) (macrop operator))
	      (funcall (cdr operator) (cdr expr) env))
	     (t (error "~a not callable" (car expr))))))
	(t (error "Unknown expression ~a" expr))))

(setq *global-env*
      (list nil ; So we can descructively push inside function
	    ;; Numeric operations
	    ;; TODO: Implement `numerator`/`denominator`
	    ;; TODO: Implement `gcd`/`lcm`
	    (cons '+ #'+)
	    (cons '- #'-)
	    (cons '* #'*)
	    (cons '/ #'/)
	    (cons '= #'=)
	    (cons 'abs #'abs)
	    (cons 'expt #'expt)
	    (cons 'modulo #'mod)
	    ;; (cons 'quotient #'floor) ; TODO: Implement correctly
	    (cons 'remainder #'rem)
	    (cons 'floor #'floor)
	    (cons 'ceiling #'ceiling)
	    (cons 'truncate #'truncate)
	    (cons 'round #'round)
	    (cons 'exact? #'rationalp)
	    (cons 'inexact? #'floatp)
	    ;; Comparison
	    (cons 'min #'min)
	    (cons 'max #'max)
	    (cons '< #'<)
	    (cons '> #'>)
	    (cons '>= #'>=)
	    (cons '<= #'<=)
	    ;; List operations
	    (cons 'cons #'cons)
	    (cons 'car #'car)
	    (cons 'cdr #'cdr)
	    (cons 'cadr #'cadr)
	    (cons 'second #'cadr)
	    (cons 'list #'list)
	    (cons 'append #'append)
	    (cons 'length #'length)
	    (cons 'apply #'apply)
	    (cons 'reverse #'reverse)
	    (cons 'list-tail #'(lambda (lst n) (nthcdr n lst)))
	    (cons 'list-ref #'(lambda (lst n) (nth n lst)))
	    ;; (cons 'memq #'(lambda (obj lst) (member obj lst :test #'eq)))
	    ;; (cons 'memv #'(lambda (obj lst) (member obj lst :test #'eq))) ; TODO: Implement correctly
	    ;; (cons 'member #'(lambda (obj lst) (member obj lst :test #'equalp)))
	    ;; Printing
	    (cons 'print #'print)
	    (cons 'display #'princ) ; TODO: It also prints output
	    (cons 'displayln #'print)
	    (cons 'newline #'(lambda () (format t "~%")))
	    (cons 'print #'print)
	    ;; Type cheking
	    (cons 'atom? #'atom)
	    (cons 'boolean? #'(lambda (x) (or (null x) (eq x t))))
	    (cons 'integer? #'integerp)
	    (cons 'list? #'listp)
	    (cons 'number? #'numberp)
	    (cons 'null? #'null)
	    (cons 'pair? #'consp)
	    (cons 'string? #'stringp)
	    (cons 'symbol? #'symbolp)
	    (cons 'char? #'characterp)
	    (cons 'vector? #'vectorp)
	    ;; (cons 'port? nil)
	    ;;
	    (cons 'make-vector #'(lambda (size &optional (v 0))
				   (make-array size :initial-element v)))
	    (cons 'vector-set! #'(lambda (vec pos v)
				   (setf (aref vec pos) v)))
	    ;;
	    (cons 'eq? #'eq)
	    (cons 'eqv? #'eql)
	    (cons 'equal? #'equalp)
	    (cons 'not #'not)
	    (cons 'string=? #'string=)
	    (cons 'error #'error)
	    (cons 'exit (constantly :quit))
	    ;;
	    (cons '#t t)
	    (cons '#f nil)
	    ;;
	    (cons 'eval #'evaluate)
	    (cons 'procedure? #'functionp)
	    ))

(defun prompt-expr ()
  (format *query-io* "λ> ")
  (force-output *query-io*)
  (read t t))

(defun load-script (filename &key quiet)
  (with-open-file (script filename)
    (let ((result nil))
      (loop
	(let ((sexp (read script nil :eof)))
	  (if (eq sexp :eof)
	      (progn
		(when (null quiet)
		  (format t "~a~%" result))
		(return))
	      (handler-case
		  (setq result (evaluate sexp *global-env*))
		(error (err) (format t "~a~%" err)))))))))

(defun repl ()
  (loop
    (handler-case
	(let ((result (evaluate (prompt-expr) *global-env*)))
	  (if (equal result :quit)
	      (return)
	      (format t "~a~%" result)))
      (end-of-file () (return))
      (error (err) (format t "~a~%" err))))
  (format t "Bye!"))

(load-script "src/scm/core.scm" :quiet t)
