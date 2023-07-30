(defpackage :cl-scheme
  (:use :cl))
(in-package :cl-scheme)

(defparameter *special-forms*
  '(define
    if
    cond
    and
    or
    let
    begin
    lambda
    quote
    quasiquote
    unquote
    mu
    define-macro
    expect
    unquote-splicing
    delay
    cons-stream
    set!)
  "Scheme special forms.")

(set-dispatch-macro-character #\# #\t #'(lambda (&rest _)
					  (declare (ignore _)) t))
(set-dispatch-macro-character #\# #\f #'(lambda (&rest _)
					  (declare (ignore _)) nil))

(defvar *global-env* nil
  "Interpreter's global environment.")

(setq *global-env*
      (list nil ; So we can descructively push inside function
	    (cons '+ #'+)
	    (cons '- #'-)
	    (cons '* #'*)
	    (cons '/ #'/)
	    (cons '= #'=)
	    (cons '< #'<)
	    (cons '> #'>)
	    (cons '>= #'>=)
	    (cons '<= #'<=)
	    (cons 'abs #'abs)
	    (cons 'min #'min)
	    (cons 'max #'max)
	    (cons 'cons #'cons)
	    (cons 'car #'car)
	    (cons 'cdr #'cdr)
	    (cons 'list #'list)
	    (cons 'length #'length)
	    (cons 'print #'print)

	    (cons '#t t)
	    (cons '#f nil)
	    ))

(defun push-cdr (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun lookup (sym env)
  (cdr (assoc sym env)))

(defstruct Procedure
  "Scheme function defined with lambda and define special forms."
  params body env)

(declaim (ftype function evaluate))

(defun create-env (bindings env)
  (loop
    for bind in bindings
    collect (cons (car bind) (funcall #'evaluate (cadr bind) env))))

(defun create-procedure-env (procedure args enclosing-env)
  (declare (ignore enclosing-env))
  (let ((params (Procedure-params procedure))
	(env    (Procedure-env procedure)))
    (append (loop
	      for sym in params
	      for val in args
	      collect (cons sym val))
	    env)))

(defun evaluate-body (body env)
  ;; TODO: This could probably be rewritten.
  (dolist (expression
	   (butlast body)
	   (progn
	     (funcall #'evaluate
		      (car (last body))
		      env)))
    (funcall #'evaluate expression env)))

(defun evaluate-special-form (form args env)
  (case form
    ((if)
     (if (= 3 (length args))
	 (if (funcall #'evaluate (car args) env)
	     (funcall #'evaluate (cadr args) env)
	     (funcall #'evaluate (caddr args) env))
	 "Error: malformed if special form")) ; Just a string for now.
    ((or)
     (let ((frst (funcall #'evaluate (car args) env)))
       (if frst
	   frst
	   (when (consp (cdr args))
	     (funcall #'evaluate
		      `(or ,@(cdr args)) env)))))
    ((and)
     (let ((frst (funcall #'evaluate (car args) env)))
       (if (null frst)
	   frst
	   (if (consp (cdr args))
	       (funcall #'evaluate `(and ,@(cdr args)) env)
	       frst))))
    ((define)
     (if (consp (car args))
	 (progn
	   (push-cdr
	    (cons (caar args)
		  (make-Procedure :params (cdar args)
				  :body (cdr args)
				  :env env))
	    env)
	   (caar args))
	 (progn
	   (push-cdr (cons (car args)
			   (funcall #'evaluate
				    (cadr args)
				    env))
		     env)
	   (car args))))
    ((cond)
     (let ((result nil))
       (loop
	 named cond-loop
	 for pair in args
	 ;; TODO: Maybe this can be rewritten
	 do (when (funcall #'evaluate (car pair) env)
	      (setq result (funcall #'evaluate (cadr pair) env))
	      (return-from cond-loop)))
       result))
    ((let)
     (let ((current-env (append
			 (create-env (car args) env)
			 env))
	   (body (cdr args)))
       (evaluate-body body current-env)))
    ((begin)
     (evaluate-body args env))
    ((lambda)
     (make-Procedure :params (car args)
		     :body (cdr args)
		     :env env))))

(defun evaluate (expr &optional (env *global-env*))
  (if (consp expr)
      (let ((root     (car expr))
	    (branches (cdr expr)))
	(if (member root *special-forms*)
	    (evaluate-special-form root branches env)
	    (let ((root-fn (lookup root env)))
	      (cond
		((functionp root-fn)
		 (apply root-fn
			(mapcar #'(lambda (form)
				    (funcall #'evaluate form env))
				branches)))
		((Procedure-p root-fn)
		 (let* ((args (mapcar #'(lambda (form)
					  (funcall #'evaluate form env))
				      branches))
			(body (Procedure-body root-fn))
			(closure (create-procedure-env root-fn args env)))
		   (evaluate-body body closure)))
		(t (format t "~a not callable~%" root))))))
      (cond
	((keywordp expr) expr)
	((symbolp expr)
	 (lookup expr env))
	(t expr))))

(defun prompt-expr ()
  (format *query-io* "> ")
  (force-output *query-io*)
  (read t t))

(defun repl ()
  (loop
    (let ((result (evaluate (prompt-expr) *global-env*)))
      (if (equal result :quit)
	  (return)
	  (format t "~a~%" result))))
  (format t "Bye!"))

(repl)