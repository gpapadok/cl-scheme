(in-package #:cl-scheme)

;; TODO: Implement letrec

(defun create-procedure (params body proc-env)
  (cons
   'procedure
   (lambda (args env)
     (evaluate-body body
                    (env-extend
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
      (evaluate-body body (env-extend params
                                      args
                                      macro-env))
      env)))) ; TODO: Probably separate macro-expansion from evaluation

(defparameter *special-forms* nil
  "Set of all possible Scheme special forms.")

(defmacro defspecial (name lambda-list &body body)
  "Defines a special form evaluation function and pushes it in
the global special form alist"
  (let ((fn-name (read-from-string
                  (concatenate 'string "EVALUATE-" (string name)))))
    `(progn
       (defun ,fn-name
         ,lambda-list
         ,@body)

       (pushnew ',name *special-forms*))))

(defconstant +quasiquote-symbol+
  (or #+SBCL 'sb-int:quasiquote
      nil)
  "Implementation specific quasiquote symbol")
(pushnew +quasiquote-symbol+ *special-forms*)

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
      (progn                ; Procedure definition
        (env-push! env
                   (caar args)
                   (create-procedure (cdar args)
                                     (cdr args)
                                     env))
        (caar args))
      (progn                ; Variable definition
        (env-push! env
                   (car args)
                   (evaluate
                    (cadr args)
                    env))
        (car args))))

(defspecial cond (args env)
  (loop
    named cond-form
    for clause in args
    do (when (evaluate (car clause) env)
         (return-from cond-form
           ;; TODO: Should work with multiple expressions for each test
           (evaluate (cadr clause) env)))))

(defspecial let (args env) ; TODO: defining same variable twice should be error
  (if (consp (car args))
      (let ((values (mapcar #'second (car args)))
            (proc (create-procedure (mapcar #'car (car args))
                                    (cdr args)
                                    env)))
        (funcall (cdr proc) values env))
      (let* ((name (car args))
             (values (mapcar #'second (second args)))
             (proc-env (env-copy env)) ; This may be inefficient
             (proc (create-procedure (mapcar #'car (second args))
                                     (cddr args)
                                     proc-env)))
        (env-push! proc-env name proc)
        (funcall (cdr proc) values proc-env))))

(defspecial let* (args env)
  (evaluate-body (cdr args)
                 (env-extend-with-bindings* env (car args))))

(defspecial letrec (args env)
  (let ((letenv (env-copy env)))
    (dolist (s (mapcar #'car (car args)))
      (env-push! letenv s nil))
    (dolist (binding (car args))
      (env-update! letenv
                  (car binding)
                  (evaluate (second binding) letenv)))
    (evaluate-body (cdr args) letenv)))

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
            (and (consp x)
                 (symbolp (car x))
                 (string= 'unquote-splicing (car x))))
        sexp))

(defspecial quasiquote (args env)
  (labels ((unquote-quasiquoted (form env)
             (if (atom form)
                 form
                 (cond ((and (symbolp (car form))
                             (string= 'unquote (car form))) ; TODO: Figure out how to add , reader macro
                        (evaluate (second form) env))
                       ((contains-comma-at-p form)
                        (flet ((sym-position (sym sexp)
                                 (position sym
                                           sexp
                                           :test #'(lambda (item x)
                                                     (and (consp x)
                                                          (symbolp (car x))
                                                          (string= item (car x)))))))
                          (let ((pos (sym-position 'unquote-splicing form)))
                            (append (unquote-quasiquoted (subseq form 0 pos) env)
                                    (evaluate (cadr (nth pos form)) env)
                                    (unquote-quasiquoted (subseq form (1+ pos)) env)))))
                       (:else
                        (cons (unquote-quasiquoted (car form) env)
                              (unquote-quasiquoted (cdr form) env)))))))
    (if (= (length args) 1)
        (unquote-quasiquoted (car args) env)
        (error "wrong number of args to quqsiquote: ~a" (length args)))))

(defspecial define-macro (args env)
  (env-push! env (caar args) (create-macro (cdar args) (cdr args) env))
  (caar args))

(defspecial set! (args env)
  (if (assoc (car args) (bindings env))
      (progn
        (env-update! env (car args) (evaluate (cadr args) env))
        (car args))
      (error "~a undefined~%" (car args))))

(defspecial set-car! (args env)
  (if-let (val (env-lookup env (car args)))
    (if (consp val)
        (env-update! env
                    (car args)
                    (cons (evaluate (cadr args) env)
                          (cdr val)))
        (error "~a not a list~%" (car args)))))

(defspecial set-cdr! (args env)
  (if-let (val (env-lookup env (car args)))
    (if (consp val)
        (env-update! env
                    (car args)
                    (cons (car val)
                          (evaluate (cadr args) env)))
        (error "~a not a list~%" (car args)))))

(defspecial case (args env)
  (let ((test (evaluate (car args) env)))
    (loop
      named case-form
      for clause in (cdr args)
      do (when (or (and (symbolp (car clause))
                        (string= 'else (car clause)))
                   (member test (car clause)))
           (return-from case-form
             ;; TODO: This should work for multiple expressions too
             (evaluate (cadr clause) env))))))

(defspecial do (args env)
  (let* ((varlist (car args))
         (endlist (second args))
         (body (cddr args))
         (params (mapcar #'car varlist))
         (args (mapcar #'second varlist)))
    (do ((doenv (env-extend params
                            (mapcar #'(lambda (arg)
                                        (evaluate arg env))
                                    args)
                            env)
                (env-extend-with-bindings
                 doenv
                 (mapcar #'(lambda (lst)
                             (list (car lst) (third lst)))
                         (remove-if-not #'third varlist)))))
        ((evaluate (car endlist) doenv)
         (evaluate-body (cdr endlist) doenv))
      (evaluate-body body doenv))))

(defspecial delay (args env)
  (if (= (length args) 1)
      (let ((evaluated nil)
            (result nil))
        (lambda ()
          (if evaluated
              result
              (setf evaluated t
                    result (evaluate (car args) env)))))
      (error "malformed delay special form~%")))

(defspecial force (args env)
  (if (= (length args) 1)
      (funcall (evaluate (car args) env))
      (error "malformed force special form")))
