
(defun lookup (sym env)
  (cdr (assoc sym env)))

(defconstant +quasiquote-symbol+
  (or #+SBCL 'sb-int:quasiquote
      nil))

(defun evaluate-body (body env)
  (dolist (expr
	   (butlast body)
	   (evaluate (car (last body)) env))
    (evaluate expr env)))

(defmacro if-let (binding-form true-expression &optional false-expression)
  `(let (,binding-form)
     (if ,(car binding-form)
	 ,true-expression
	 ,false-expression)))

(defun eval-if (args env)
  (if (<= 2 (length args) 3)
      (if (evaluate (car args) env)
	  (evaluate (cadr args) env)
	  (evaluate (caddr args) env))
      (error "malformed if special form")))

(defun eval-or (args env)
  ;; TODO: rename `frst`
  (if-let (frst (evaluate (car args) env))
    frst
    (when (consp (cdr args))
      (funcall #'evaluate
	       `(or ,@(cdr args)) env))))

(defun eval-and (args env)
  (let ((frst (evaluate (car args) env)))
    (if (and frst (consp (cdr args)))
	(evaluate `(and ,@(cdr args)) env)
	frst)))

(defun eval-define (args env)
  (if (consp (car args))
      (progn				; Procedure definition
	(push-cdr (cons (caar args)
			(create-procedure (cdar args)
					  (cdr args)
					  env))
		  env)
	(caar args))
      (progn				; Variable definition
	(push-cdr (cons (car args)
			(evaluate
			 (cadr args)
			 env))
		  env)
	(car args))))

(defun eval-cond (args env)
  (loop
    named cond-form
    for clause in args
    do (when (evaluate (car clause) env)
	 (return-from cond-form
	   ;; TODO: Should work with multiple expressions for each test
	   (evaluate (cadr clause) env)))))

(defun eval-let (args env)
  (evaluate-body (cdr args)
		 (extend-env-with-bindings (car args) env)))

(defun eval-let* (args env)
  (evaluate-body (cdr args)
		 (extend-env-with-bindings* (car args) env)))

(defun eval-begin (args env)
  (evaluate-body args env))

(defun eval-lambda (args env)
  (create-procedure (car args) (cdr args) env))

(defun eval-quote (args env)
  (declare (ignore env))
  (if (= (length args) 1)
      (car args)
      (error "wrong number of args to quote: ~a" (length args))))

(defun eval-quasiquote (args env)
  (if (= (length args) 1)
      (unquote-quasiquoted (car args) env)
      (error "wrong number of args to quqsiquote: ~a" (length args))))

(defun eval-define-macro (args env)
  (progn
    (push-cdr (cons (caar args)
		    (create-macro (cdar args) (cdr args) env))
	      env)
    (caar args)))

(defun eval-set! (args env)
  (if (assoc (car args) env)
      (progn
	(update-env (car args) (evaluate (cadr args) env) env)
	(car args))
      (error "~a undefined~%" (car args))))

(defun eval-set-car! (args env)
  (if-let (val (lookup (car args) env))
    (if (consp val)
	(update-env (car args)
		    (cons (evaluate (cadr args) env)
			  (cdr val))
		    env)
	(error "~a not a list~%" (car args)))))

(defun eval-set-cdr! (args env)
  (if-let (val (lookup (car args) env))
    (if (consp val)
	(update-env (car args)
		    (cons (car val)
			  (evaluate (cadr args) env))
		    env)
	(error "~a not a list~%" (car args)))))

(defun eval-case (args env)
  (let ((test (evaluate (car args))))
    (loop
      named case-form
      for clause in (cdr args)
      do (when (or (eql 'else (car clause)) (member test (car clause)))
	   (return-from case-form
	     ;; TODO: This should work for multiple expressions too
	     (evaluate (cadr clause) env))))))

(defun eval-do (args env)
  (let* ((varlist (car args))
	 (endlist (second args))
	 (body (cddr args))
	 (params (mapcar #'car varlist))
	 (args (mapcar #'second varlist)))
    (do ((doenv (extend-env params
			    (mapcar #'(lambda (arg)
					(evaluate arg env))
				    args)
			    env)
		(extend-env-with-bindings
		 (->> varlist
		   (remove-if-not #'third)
		   (mapcar #'(lambda (lst)
			       (list (car lst) (third lst)))))
		 doenv)))
	((evaluate (car endlist) doenv)
	 (evaluate-body (cdr endlist) doenv))
      (evaluate-body body doenv))))

(defparameter +special-forms+
  (list
   (cons 'if #'eval-if)
   (cons 'or #'eval-or)
   (cons 'and #'eval-and)
   (cons 'define #'eval-define)
   (cons 'cond #'eval-cond)
   (cons 'let #'eval-let)
   (cons 'let* #'eval-let*)
   (cons 'begin #'eval-begin)
   (cons 'lambda #'eval-lambda)
   (cons 'quote #'eval-quote)
   (cons 'quasiquote #'eval-quasiquote)
   (cons +quasiquote-symbol+ #'eval-quasiquote)
   (cons 'define-macro #'eval-define-macro)
   (cons 'set! #'eval-set!)
   (cons 'set-car! #'eval-set-car!)
   (cons 'set-cdr! #'eval-set-cdr!)
   (cons 'case #'eval-case)
   (cons 'do #'eval-do)))
