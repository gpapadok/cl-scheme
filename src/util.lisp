(in-package #:cl-scheme)

(declaim (ftype function evaluate))

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

(defun push-cdr (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun update-env (sym value env)
  (setf (cdr (assoc sym env)) value))
