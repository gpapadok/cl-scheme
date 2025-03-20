(in-package #:cl-scheme)

(defun procedurep (item)
  (and (consp item) (eq 'procedure (car item))))

(defun macrop (item)
  (and (consp item) (eq 'macro (car item))))

(defun special-form-p (expr)
  (and (consp expr)
       (symbolp (car expr))
       (member (car expr) *special-forms* :test #'string=)))

(defun evaluate-special-form (expr env)
  (if (member (car expr) *special-forms* :test #'string=)
    (funcall (intern (concatenate 'string "EVALUATE-" (string (car expr))) :cl-scheme)
             (cdr expr) env)
    (error "Form ~a not implemented~%" (car expr))))

(defun self-evaluating-p (expr)
  (or (numberp expr) (stringp expr) (keywordp expr) (vectorp expr)))

(defun variablep (expr) (symbolp expr))

(defun applicationp (expr) (consp expr))

(defun lookup-variable (expr env) (env-lookup env expr))

(defun evaluate (expr env)
  (cond ((eq expr :env) (print env))
        ((self-evaluating-p expr) expr)
        ((special-form-p expr) (evaluate-special-form expr env))
        ((variablep expr) (lookup-variable expr env))
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

(defun evaluate-body (body env)
  (dolist (expr
           (butlast body)
           (evaluate (car (last body)) env))
    (evaluate expr env)))

(defun prompt-expr ()
  (format *query-io* "λ> ")
  (force-output *query-io*)
  (read t t))

(defun load-script (filename env &key quiet)
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
                  (setq result (evaluate sexp env))
                (error (err) (format t "~a~%" err)))))))))

(defun repl ()
  (let ((env (create-global-env :alist-env)))
    (loop
      (handler-case
          (let ((result (evaluate (prompt-expr) env)))
            (if (equal result :quit)
                (return)
                (format t "~a~%" result)))
        (end-of-file () (return))
        (error (err) (format t "~a~%" err))))
    (format t "Bye!")))
