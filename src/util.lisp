(in-package #:cl-scheme)

(defmacro if-let (binding-form true-expression &optional false-expression)
  `(let (,binding-form)
     (if ,(car binding-form)
         ,true-expression
         ,false-expression)))

(defun push-cdr! (obj place)
  (setf (cdr place) (cons obj (cdr place))))

(defun if-condition (form) (car form))
(defun if-then (form) (second form))
(defun if-else (form) (third form))

(defun clause-predicate (form) (car form))
(defun clause-body (form) (second form))

(defun lambda-parameters (form) (car form))
(defun lambda-body (form) (cdr form))

(defun form-of-quote (form) (car form))

(defun contains-comma@-p (sexp)
  (some #'(lambda (x)
            (and (consp x)
                 (symbolp (car x))
                 (string= 'unquote-splicing (car x))))
        sexp))

(defun assignment-variable (form) (car form))
(defun assignment-value (form) (second form))
