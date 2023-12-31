;;; Scheme implementation in Common Lisp

(ql:quickload :arrow-macros)
(use-package :arrow-macros)

(load "special-forms.lisp")

;; Use cl built-ins for booleans
(set-dispatch-macro-character #\# #\t #'(lambda (stream subchar arg)
					  (declare (ignore stream
							   subchar
							   arg))
					  t))
(set-dispatch-macro-character #\# #\f #'(lambda (stream subchar arg)
					  (declare (ignore stream
							   subchar
							   arg))
					  nil))

(defvar *global-env* nil
  "Interpreter's global environment.")

(defun procedurep (item)
  (and (consp item) (eq 'procedure (car item))))

(defun macrop (item)
  (and (consp item) (eq 'macro (car item))))

(defun special-form-p (expr)
  (member (car expr) (mapcar #'car *special-forms*)))

(defun evaluate-special-form (expr env)
  (if-let (eval-op (lookup (car expr) *special-forms*))
    (funcall eval-op (cdr expr) env)
    (error "Form ~a not implemented~%" (car expr))))

(defun evaluate (expr &optional (env *global-env*))
  (cond ((consp expr)
	 (if (special-form-p expr)
	     (evaluate-special-form expr env)
	     (let ((operator (evaluate (car expr) env)))
	       (cond
		 ((functionp operator)
		  (apply operator
			 (mapcar #'(lambda (form)
				     (funcall #'evaluate form env))
				 (cdr expr))))
		 ((or (procedurep operator) (macrop operator))
		  (funcall (cdr operator) (cdr expr) env))
		 (t (error "~a not callable" (car expr)))))))
	((keywordp expr) expr)
	((symbolp expr) (lookup expr env))
	((atom expr) expr)
	(t (error "~a this should never trigger" expr))))

(setq *global-env*
      (list nil ; So we can descructively push inside function
	    ;; Numeric operations
	    (cons '+ #'+)
	    (cons '- #'-)
	    (cons '* #'*)
	    (cons '/ #'/)
	    (cons '= #'=)
	    (cons 'abs #'abs)
	    (cons 'expt #'expt)
	    (cons 'modulo #'mod)
	    (cons 'quotient #'floor)
	    (cons 'remainder #'rem)
	    ;; Comparison
	    (cons 'min #'min)
	    (cons 'max #'max)
	    (cons '< #'<)
	    (cons '> #'>)
	    (cons '>= #'>=)
	    (cons '<= #'<=)
	    (cons 'even? #'evenp)
	    (cons 'odd? #'oddp)
	    (cons 'zero? #'zerop)
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
	    (cons 'pair? #'(lambda (x) (and (car x) (cdr x) t)))
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

(load-script "core.scm" :quiet t)
