;;; Scheme in Common Lisp

(defconstant +quasiquote-symbol+
  (or #+SBCL 'sb-int:quasiquote
      nil))

(defparameter *special-forms*
  `(if
    define
    cond
    case
    and
    or
    let
    let*
    letrec ; TODO: implement
    begin
    lambda
    quote
    quasiquote
    ,+quasiquote-symbol+
    unquote
    define-macro
    unquote-splicing
    ;; delay
    ;; cons-stream
    set!
    set-car!
    set-cdr!)
  "Scheme special forms.")

(defmacro if-let (binding-form true-expression &optional false-expression)
  `(let (,binding-form)
     (if ,(car binding-form)
	 ,true-expression
	 ,false-expression)))

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
;; do
;; named let

(defvar *global-env* nil
  "Interpreter's global environment.")

(defun push-cdr (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun lookup (sym env)
  (cdr (assoc sym env)))

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

(defun evaluate-body (body env)
  (dolist (expr
	   (butlast body)
	   (funcall #'evaluate (car (last body)) env))
    (funcall #'evaluate expr env)))

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
  (case form
    ((if)
     (if (<= 2 (length args) 3)
	 (if (funcall #'evaluate (car args) env)
	     (funcall #'evaluate (cadr args) env)
	     (funcall #'evaluate (caddr args) env))
	 (error "malformed if special form")))
    ((or) ;; TODO: rename `frst`
     (if-let (frst (funcall #'evaluate (car args) env))
       frst
       (when (consp (cdr args))
	 (funcall #'evaluate
		  `(or ,@(cdr args)) env))))
    ((and)
     (let ((frst (funcall #'evaluate (car args) env)))
       (if (and frst (consp (cdr args)))
	   (funcall #'evaluate `(and ,@(cdr args)) env)
	   frst)))
    ((define)
     (if (consp (car args))
	 (progn ; Procedure definition
	   (push-cdr (cons (caar args)
			   (create-procedure (cdar args)
					     (cdr args)
					     env))
		     env)
	   (caar args))
	 (progn ; Variable definition
	   (push-cdr (cons (car args)
			   (funcall #'evaluate
				    (cadr args)
				    env))
		     env)
	   (car args))))
    ((cond) ; TODO: Add `else` and `=>`
     (loop
       named cond-form
       for clause in args
       do (when (funcall #'evaluate (car clause) env)
	    (return-from cond-form
	      ;; TODO: Should work with multiple expressions for each test
	      (funcall #'evaluate (cadr clause) env)))))
    ((let)
     (evaluate-body (cdr args)
		    (extend-env-with-bindings (car args) env)))
    ((let*)
     (evaluate-body (cdr args)
		    (extend-env-with-bindings* (car args) env)))
    ((begin)
     (evaluate-body args env))
    ((lambda) ; TODO: Add argument destructuring
     (create-procedure (car args) (cdr args) env))
    ((quote)
     (if (= (length args) 1)
	 (car args)
	 (error "wrong number of args to quote: ~a" (length args))))
    (`(or quasiquote ,+quasiquote-symbol+)
     (if (= (length args) 1)
	 (unquote-quasiquoted (car args) env)
	 (error "wrong number of args to quqsiquote: ~a" (length args))))
    ((define-macro)
     (progn
       (push-cdr (cons (caar args)
		       (create-macro (cdar args) (cdr args) env))
		 env)
       (caar args)))
    ((set!) ; TODO: Correct error handling
     (if (assoc (car args) env)
	 (progn
	   (update-env (car args) (funcall #'evaluate (cadr args) env) env)
	   (car args))
	 (error "~a undefined~%" (car args))))
    ((set-car!) ; TODO: Maybe these don't have to be special forms
     (if-let (val (lookup (car args) env))
       (if (consp val)
	   (update-env (car args)
		       (cons (funcall #'evaluate (cadr args) env)
			     (cdr val))
		       env)
	   (error "~a not a list~%" (car args)))))
    ((set-cdr!)
     (if-let (val (lookup (car args) env))
       (if (consp val)
	   (update-env (car args)
		       (cons (car val)
			     (funcall #'evaluate (cadr args) env))
		       env)
	   (error "~a not a list~%" (car args)))))
    ((case)
     (let ((test (funcall #'evaluate (car args))))
       (loop
	 named case-form
	 for clause in (cdr args)
	 do (when (or (eql 'else (car clause)) (member test (car clause)))
	      (return-from case-form
		;; TODO: This should work for multiple expressions too
		(funcall #'evaluate (cadr clause) env))))))
    ))

(defun evaluate (expr &optional (env *global-env*))
  (if (consp expr)
      (let ((root (car expr)) ; TODO: Rename some variables
	    (branches (cdr expr)))
	(if (member root *special-forms*)
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
