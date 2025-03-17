(in-package #:cl-scheme)

(defun procedurep (item)
  (and (consp item) (eq 'procedure (car item))))

(defun macrop (item)
  (and (consp item) (eq 'macro (car item))))

(defun special-form-p (expr)
  (and (consp expr)
       (symbolp (car expr))
       (member (car expr) (mapcar #'car *special-forms*) :test #'string=)))

(defun evaluate-special-form (expr env)
  (if-let (eval-op (env-lookup *special-forms* (car expr)))
    (funcall eval-op (cdr expr) env)
    (error "Form ~a not implemented~%" (car expr))))

(defun self-evaluating-p (expr)
  (or (numberp expr) (stringp expr) (keywordp expr) (vectorp expr)))

(defun variablep (expr) (symbolp expr))

(defun applicationp (expr) (consp expr))

(defun lookup-variable (expr env) (env-lookup env expr))

(defun evaluate (expr &optional (env *global-env*))
  (cond ((self-evaluating-p expr) expr)
	    ((variablep expr) (lookup-variable expr env))
	    ((special-form-p expr) (evaluate-special-form expr env))
        ((applicationp expr)
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
        ((atom expr) expr)
	    (t (error "Unknown expression ~a" expr))))

(env-push 'evaluate #'evaluate *global-env*)

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
