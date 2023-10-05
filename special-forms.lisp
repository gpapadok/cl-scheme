
(declaim (ftype function evaluate)
	 (ftype function evaluate-body)
	 (ftype function extend-env))

(defparameter *special-forms*
  nil)

(defmacro defspecial (name lambda-list &body body)
  "Defines a special form evaluation function and pushes it in
the global special form alist"
  (let ((fn-name (read-from-string
		  (concatenate 'string "eval-" (string name)))))
    `(progn
       (defun ,fn-name
	 ,lambda-list
	 ,@body)

       (push (cons ',name #',fn-name)
	     *special-forms*))))

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

(defspecial if (args env)
  (if (<= 2 (length args) 3)
      (if (evaluate (car args) env)
	  (evaluate (cadr args) env)
	  (evaluate (caddr args) env))
      (error "malformed if special form")))

(defspecial or (args env)
  ;; TODO: rename `frst`
  (if-let (frst (evaluate (car args) env))
    frst
    (when (consp (cdr args))
      (funcall #'evaluate
	       `(or ,@(cdr args)) env))))

(defspecial and (args env)
  (let ((frst (evaluate (car args) env)))
    (if (and frst (consp (cdr args)))
	(evaluate `(and ,@(cdr args)) env)
	frst)))

(defspecial define (args env)
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

(defspecial cond (args env)
  (loop
    named cond-form
    for clause in args
    do (when (evaluate (car clause) env)
	 (return-from cond-form
	   ;; TODO: Should work with multiple expressions for each test
	   (evaluate (cadr clause) env)))))

(defspecial let (args env)
  (evaluate-body (cdr args)
		 (extend-env-with-bindings (car args) env)))

(defspecial let* (args env)
  (evaluate-body (cdr args)
		 (extend-env-with-bindings* (car args) env)))

(defspecial begin (args env)
  (evaluate-body args env))

(defspecial lambda (args env)
  (create-procedure (car args) (cdr args) env))

(defspecial quote (args env)
  (declare (ignore env))
  (if (= (length args) 1)
      (car args)
      (error "wrong number of args to quote: ~a" (length args))))

(defspecial quasiquote (args env)
  (if (= (length args) 1)
      (unquote-quasiquoted (car args) env)
      (error "wrong number of args to quqsiquote: ~a" (length args))))
;; Implementation specific quasiquote symbol
(push (cons +quasiquote-symbol+ #'eval-quasiquote) *special-forms*)

(defspecial define-macro (args env)
  (push-cdr (cons (caar args)
		  (create-macro (cdar args) (cdr args) env))
	    env)
  (caar args))

(defspecial set! (args env)
  (if (assoc (car args) env)
      (progn
	(update-env (car args) (evaluate (cadr args) env) env)
	(car args))
      (error "~a undefined~%" (car args))))

(defspecial set-car! (args env)
  (if-let (val (lookup (car args) env))
    (if (consp val)
	(update-env (car args)
		    (cons (evaluate (cadr args) env)
			  (cdr val))
		    env)
	(error "~a not a list~%" (car args)))))

(defspecial set-cdr! (args env)
  (if-let (val (lookup (car args) env))
    (if (consp val)
	(update-env (car args)
		    (cons (car val)
			  (evaluate (cadr args) env))
		    env)
	(error "~a not a list~%" (car args)))))

(defspecial case (args env)
  (let ((test (evaluate (car args))))
    (loop
      named case-form
      for clause in (cdr args)
      do (when (or (eql 'else (car clause)) (member test (car clause)))
	   (return-from case-form
	     ;; TODO: This should work for multiple expressions too
	     (evaluate (cadr clause) env))))))

(defspecial do (args env)
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
