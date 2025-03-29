(in-package #:cl-scheme)

(defparameter *special-forms* nil
  "Set of all possible Scheme special forms.")

(defun procedurep (item)
  (and (consp item) (eq 'procedure (tag item))))

(defun macrop (item)
  (and (consp item) (eq 'macro (tag item))))

(defun special-form-p (expr)
  (and (consp expr)
       (symbolp (car expr))
       (member (car expr) *special-forms* :test #'string=)))

(defun evaluate-special-form (expr env)
  (if (member (car expr) *special-forms* :test #'string=)
      (funcall (intern (concatenate 'string "EVALUATE-" (string (car expr))) :cl-scheme)
               (cdr expr) env)
      (error "Form ~a not implemented~%" (car expr))))

(defun evaluate-arguments (args env)
  (mapcar #'(lambda (form)
              (evaluate form env))
          args))

(defun evaluate-application (expr env) ; TODO: Can this be simplified?
  (let ((operator (evaluate (expression-operator expr) env)))
    (cond
      ((functionp operator)
       (apply operator
              (evaluate-arguments (expression-arguments expr) env)))
      ((or (procedurep operator) (macrop operator))
       (funcall (operator-lambda operator) (expression-arguments expr) env))
      (t (error "~a not callable" (expression-operator expr))))))

(defun self-evaluating-p (expr)
  (or (numberp expr) (stringp expr) (keywordp expr) (vectorp expr)))

(defun variablep (expr) (symbolp expr))

(defun applicationp (expr) (consp expr))

(defun lookup-variable (expr env) (env-lookup env expr))

(defun evaluate (expr env)
  (cond
    ((self-evaluating-p expr) expr)
    ((special-form-p expr) (evaluate-special-form expr env))
    ((variablep expr) (lookup-variable expr env))
    ((applicationp expr) (evaluate-application expr env))
    ((atom expr) expr)
    (t (error "Unknown expression ~a" expr))))

(defun evaluate-body (body env)
  (let ((result (evaluate (car body) env)))
    (if (null (cdr body))
        result
        (evaluate-body (cdr body) env))))

(defun prompt-expr ()
  (format *query-io* "λ> ")
  (force-output *query-io*)
  (read t t))

;; TODO: Separate file for load
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

(defun load-scheme-files (env &key quiet)
  (loop for file in (uiop:directory-files (system-relative-pathname +scheme-path+))
        do (load-script file env :quiet quiet)))

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
