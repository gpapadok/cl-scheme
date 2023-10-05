;;; Scheme in Common Lisp
(ql:quickload :arrow-macros)
(use-package :arrow-macros)

(load "special-forms.lisp")

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

(declaim (ftype function evaluate)
	 (ftype function evaluate-body)
	 (ftype function extend-env))

;; TODO
;; named let

(defvar *global-env* nil
  "Interpreter's global environment.")

(defun push-cdr (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun update-env (sym value env)
  (setf (cdr (assoc sym env)) value))

(defun create-procedure (params body proc-env)
  (cons
   'procedure
   (lambda (args &optional (env *global-env*))
     (evaluate-body body
		    (extend-env
		     params
		     (mapcar #'(lambda (form)
				 (funcall #'evaluate form env))
			     args)
		     proc-env)))))

(defun procedurep (item)
  (and (consp item) (eq 'procedure (car item))))

(defun create-macro (params body macro-env)
  (cons
   'macro
   (lambda (args &optional (env *global-env*))
     (evaluate
      (evaluate-body body (extend-env params
				      args
				      macro-env))
      env)))) ; TODO: Probably separate macro-expansion from evaluation

(defun macrop (item)
  (and (consp item) (eq 'macro (car item))))

(defun extend-env-with-bindings (bindings env)
  (cons nil
	(append (loop
		  for bind in bindings
		  collect (cons (car bind)
				(funcall #'evaluate (cadr bind) env)))
		(cdr env))))

(defun extend-env-with-bindings* (bindings env)
  (if-let (bind (car bindings))
    (extend-env-with-bindings*
     (cdr bindings)
     (append
      (list nil
	    (cons (car bind)
		  (funcall #'evaluate (cadr bind) env)))
      (cdr env)))
    env))

(defun extend-env (params args env)
  (cons nil
	(append (loop
		  for sym in params
		  for val in args
		  collect (cons sym val))
		(cdr env))))

(defun contains-comma-at-p (sexp)
  (some #'(lambda (x)
	    (and (consp x) (eq 'unquote-splicing (car x))))
	sexp))

(defun sym-position (sym sexp)
  (position sym
	    sexp
	    :test #'(lambda (item x)
		      (and (consp x) (eq item (car x))))))

(defun unquote-quasiquoted (form env)
  (if (atom form)
      form
      (cond ((eq 'unquote (car form)) ; TODO: Figure out how to add , reader macro
	     (funcall #'evaluate (cadr form) env))
	    ((contains-comma-at-p form)
	     (let ((pos (sym-position 'unquote-splicing form)))
	       (append (unquote-quasiquoted (subseq form 0 pos) env)
		       (funcall #'evaluate (cadr (nth pos form)))
		       (unquote-quasiquoted (subseq form (1+ pos)) env))))
	    (:else
	     (cons (unquote-quasiquoted (car form) env)
		   (unquote-quasiquoted (cdr form) env))))))

(defun evaluate-special-form (form args env)
  (if-let (eval-op (lookup form *special-forms*))
    (funcall eval-op args env)
    (error "Form ~a not implemented~%" form)))

(defun evaluate (expr &optional (env *global-env*))
  (if (consp expr)
      (let ((root (car expr)) ; TODO: Rename some variables
	    (branches (cdr expr)))
	(if (member root (mapcar #'car *special-forms*))
	    (evaluate-special-form root branches env)
	    (let ((operator (if (consp root)
				(evaluate root env)
				(lookup root env))))
	      (cond
		((functionp operator)
		 (apply operator
			(mapcar #'(lambda (form)
				    (funcall #'evaluate form env))
				branches)))
		((or (procedurep operator) (macrop operator))
		 (funcall (cdr operator) branches env))
		(t (error "~a not callable" root))))))
      (cond
	((keywordp expr) expr)
	((symbolp expr) (lookup expr env)) ; TODO: Error on undefined symbol
	(t expr))))

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
	    (cons 'make-vector #'(lambda (size &optional (v 0))
				   (make-array size :initial-element v)))
	    (cons 'vector-set! #'(lambda (vec pos v)
				   (setf (aref vec pos) v)))
	    ;; (cons 'port? nil)
	    ;; General
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
	    (cons 'procedure? #'procedurep)
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
