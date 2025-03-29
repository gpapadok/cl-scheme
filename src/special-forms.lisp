(in-package #:cl-scheme)

;; TODO: Implement letrec
;; TODO: Custom condition for malformed special form
;; TODO: Malformation should be check in macro
;; TODO: Implement named let
;; TODO: Should let be a non derived expression? Ponder

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
(eval-when (:compile-toplevel :load-toplevel)
  (pushnew +quasiquote-symbol+ *special-forms*))

(defspecial if (args env)
  (if (<= 2 (length args) 3)
      (if (evaluate (if-condition args) env)
          (evaluate (if-then args) env)
          (evaluate (if-else args) env))
      (error "wrong number of args to if: ~a" (length args))))

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
  (if (valid-define-args-p args)
      (env-define! env
                   (definition-variable args)
                   (evaluate (definition-value args) env))
      (error "malformed define form")))

(defspecial cond (args env)
  (labels ((recur (clauses)
             (if (null clauses)
                 nil
                 (let ((clause (car clauses)))
                   (if (evaluate (clause-predicate clause) env)
                       ;; TODO: Should work with multiple expressions for each test
                       (evaluate (clause-body clause) env)
                       (recur (cdr clauses)))))))
    (recur args)))

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
        (env-define! proc-env name proc)
        (funcall (cdr proc) values proc-env))))

(defspecial let* (args env)
  (evaluate-body (cdr args)
                 (env-extend-with-bindings* env (car args))))

(defspecial letrec (args env)
  (let ((letenv (env-copy env)))
    (dolist (s (mapcar #'car (car args)))
      (env-define! letenv s nil))
    (dolist (binding (car args))
      (env-set! letenv
                  (car binding)
                  (evaluate (second binding) letenv)))
    (evaluate-body (cdr args) letenv)))

(defspecial begin (args env)
  (evaluate-body args env))

(defspecial lambda (args env)
  (create-procedure (lambda-parameters args)
                    (lambda-body args)
                    env))

(defspecial quote (args env)
  (declare (ignore env))
  (if (= (length args) 1)
      (form-of-quote args)
      (error "wrong number of args to quote: ~a" (length args))))

(defspecial quasiquote (args env)
  (labels ((unquote-quasiquoted (form env)
             (if (atom form)
                 form
                 (cond ((and (symbolp (car form))
                             (string= 'unquote (car form))) ; TODO: Figure out how to add , reader macro
                        (evaluate (second form) env))
                       ((contains-comma@-p form)
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
  (env-define! env (caar args) (create-macro (cdar args) (cdr args) env))
  (caar args))

(defspecial set! (args env)
  (if (= (length args) 2)
      (if (env-lookup env (assignment-variable args))
          (env-set! env (assignment-variable args) (evaluate (assignment-value args) env))
          (error "~a undefined~%" (car args)))
      (error "wrong number of args to set!: ~a" (length args))))

(defspecial set-car! (args env)
  (if (= (length args) 2)
      (if-let (val (env-lookup env (assignment-variable args)))
        (if (consp val)
            (env-set! env
                         (assignment-variable args)
                         (cons (evaluate (assignment-value args) env)
                               (cdr val)))
            (error "~a not a list~%" (assignment-variable args))))
      (error "wrong number of args to set-car!: ~a" (length args))))

(defspecial set-cdr! (args env)
  (if (= (length args) 2)
      (if-let (val (env-lookup env (assignment-variable args)))
        (if (consp val)
            (env-set! env
                         (assignment-variable args)
                         (cons (car val)
                               (evaluate (assignment-value args) env)))
            (error "~a not a list~%" (assignment-variable args))))
      (error "wrong number of args to set-cdr!: ~a" (length args))))

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
