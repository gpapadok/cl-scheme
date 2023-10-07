;;; Special forms

(load "util.lisp")

(defun extend-env (params args env)
  (cons nil
	(append (loop
		  for sym in params
		  for val in args
		  collect (cons sym val))
		(cdr env))))

(defun create-procedure (params body proc-env)
  (cons
   'procedure
   (lambda (args env)
     (evaluate-body body
		    (extend-env
		     params
		     (mapcar #'(lambda (form)
				 (funcall #'evaluate form env))
			     args)
		     proc-env)))))

(defun create-macro (params body macro-env)
  (cons
   'macro
   (lambda (args env)
     (evaluate
      (evaluate-body body (extend-env params
				      args
				      macro-env))
      env)))) ; TODO: Probably separate macro-expansion from evaluation

(defun extend-env-with-bindings (bindings env)
  (cons nil
	(append (loop
		  for bind in bindings
		  collect (cons (car bind)
				(evaluate (cadr bind) env)))
		(cdr env))))

(defun extend-env-with-bindings* (bindings env)
  (if-let (bind (car bindings))
    (extend-env-with-bindings*
     (cdr bindings)
     (append
      (list nil
	    (cons (car bind)
		  (evaluate (cadr bind) env)))
      (cdr env)))
    env))

(defun push-cdr (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun update-env (sym value env)
  (setf (cdr (assoc sym env)) value))

(defparameter *special-forms* nil
  "Special forms alist to map symbol with behavior function")

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

(defconstant +quasiquote-symbol+
  (or #+SBCL 'sb-int:quasiquote
      nil)
  "Implementation specific quasiquote symbol")

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
  (if (consp (car args))
      (let ((values (mapcar #'second (car args)))
	    (proc (create-procedure (mapcar #'car (car args))
				    (cdr args)
				    env)))
	(funcall (cdr proc) values env))
      (let* ((name (car args))
	     (values (mapcar #'second (second args)))
	     (proc-env (copy-list env)) ; This may be inefficient
	     (proc (create-procedure (mapcar #'car (second args))
				    (cddr args)
				    proc-env)))
	(push-cdr (cons name proc) proc-env)
	(funcall (cdr proc) values proc-env))))

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

(defun contains-comma-at-p (sexp)
  (some #'(lambda (x)
	    (and (consp x) (eq 'unquote-splicing (car x))))
	sexp))

(defspecial quasiquote (args env)
  (labels ((unquote-quasiquoted (form env)
	     (if (atom form)
		 form
		 (cond ((eq 'unquote (car form)) ; TODO: Figure out how to add , reader macro
			(evaluate (second form) env))
		       ((contains-comma-at-p form)
			(flet ((sym-position (sym sexp)
				 (position sym
					   sexp
					   :test #'(lambda (item x)
						     (and (consp x)
							  (eq item (car x)))))))
			  (let ((pos (sym-position 'unquote-splicing form)))
			    (append (unquote-quasiquoted (subseq form 0 pos) env)
				    (evaluate (cadr (nth pos form)))
				    (unquote-quasiquoted (subseq form (1+ pos)) env)))))
		       (:else
			(cons (unquote-quasiquoted (car form) env)
			      (unquote-quasiquoted (cdr form) env)))))))
    (if (= (length args) 1)
	(unquote-quasiquoted (car args) env)
	(error "wrong number of args to quqsiquote: ~a" (length args)))))
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
