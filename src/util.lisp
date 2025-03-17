(in-package #:cl-scheme)

(defmacro if-let (binding-form true-expression &optional false-expression)
  `(let (,binding-form)
     (if ,(car binding-form)
         ,true-expression
         ,false-expression)))

(defun push-cdr! (obj place)
  (setf (cdr place) (cons obj (cdr place))))
