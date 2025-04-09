(in-package #:cl-scheme)

(defclass alist-env ()
  ((bindings
    :initarg :frames
    :initform (list)
    :accessor env-frames)))

(defmethod create-env ((env-type (eql :alist-env)))
  (make-instance 'alist-env))

(defmethod create-global-env ((env-type (eql :alist-env)))
  (let ((env (make-instance
              'alist-env
              :frames
              (list
               (list
                ;; Numeric operations
                ;; TODO: Implement `numerator`/`denominator`
                ;; TODO: Implement `gcd`/`lcm`
                (cons '+ #'+)
                (cons '- #'-)
                (cons '* #'*)
                (cons '/ #'/)
                (cons '= #'=)
                (cons 'abs #'abs)
                (cons 'expt #'expt)
                (cons 'modulo #'mod)
                ;; (cons 'quotient #'floor) ; TODO: Implement correctly
                (cons 'remainder #'rem)
                (cons 'floor #'floor)
                (cons 'ceiling #'ceiling)
                (cons 'truncate #'truncate)
                (cons 'round #'round)
                (cons 'exact? #'rationalp)
                (cons 'inexact? #'floatp)
                ;; Comparison
                (cons 'min #'min)
                (cons 'max #'max)
                (cons '< #'<)
                (cons '> #'>)
                (cons '>= #'>=)
                (cons '<= #'<=)
                ;; List operations
                (cons 'cons #'cons)
                (cons 'car #'car)
                (cons 'cdr #'cdr)
                (cons 'cadr #'cadr)
                (cons 'second #'cadr)
                (cons 'list #'list)
                (cons 'append #'append)
                (cons 'length #'length)
                (cons 'apply #'apply)
                (cons 'reverse #'reverse)
                (cons 'list-tail #'(lambda (lst n) (nthcdr n lst)))
                (cons 'list-ref #'(lambda (lst n) (nth n lst)))
                ;; (cons 'memq #'(lambda (obj lst) (member obj lst :test #'eq)))
                ;; (cons 'memv #'(lambda (obj lst) (member obj lst :test #'eq))) ; TODO: Implement correctly
                ;; (cons 'member #'(lambda (obj lst) (member obj lst :test #'equalp)))
                ;; Printing
                (cons 'print #'print)
                (cons 'display #'princ) ; TODO: It also prints output
                (cons 'displayln #'print)
                (cons 'newline #'(lambda () (format t "~%")))
                (cons 'print #'print)
                ;; Type cheking
                (cons 'atom? #'atom)
                (cons 'boolean? #'(lambda (x) (or (null x) (eq x t))))
                (cons 'integer? #'integerp)
                (cons 'list? #'listp)
                (cons 'number? #'numberp)
                (cons 'null? #'null)
                (cons 'pair? #'consp)
                (cons 'string? #'stringp)
                (cons 'symbol? #'symbolp)
                (cons 'char? #'characterp)
                (cons 'vector? #'vectorp)
                ;; (cons 'port? nil)
                ;;
                (cons 'make-vector #'(lambda (size &optional (v 0))
				                       (make-array size :initial-element v)))
                (cons 'vector-set! #'(lambda (vec pos v)
				                       (setf (aref vec pos) v)))
                ;;
                (cons 'eq? #'eq)
                (cons 'eqv? #'eql)
                (cons 'equal? #'equalp)
                (cons 'not #'not)
                (cons 'string=? #'string=)
                (cons 'error #'error)
                (cons 'exit (constantly :quit))
                ;;
                (cons '#t t)
                (cons '#f nil)
                ;;
                (cons 'procedure? #'functionp)
                (cons 'evaluate #'evaluate))))))
    (load-script (asdf:system-relative-pathname :cl-scheme "src/scm/core.scm") env :quiet t)
    env))

(defun the-empty-env () (list nil))

(defmethod first-frame ((env alist-env))
  (car (env-frames env)))

(defmethod enclosing-env ((env alist-env))
  (make-instance 'alist-env
                 :frames (cdr (env-frames env))))

(defmethod env-define! ((env alist-env) name value)
  (setf (env-frames env) (cons (bind-to-frame (first-frame env) name value)
                               (cdr (env-frames env))))
  (env-frames env))

(defmethod empty-env-p ((env alist-env))
  (null (first-frame env)))

(defmethod env-lookup ((env alist-env) sym)
  (if (empty-env-p env)
      (error "~a undefined" sym)
      (multiple-value-bind (value ok) (frame-lookup (first-frame env) sym)
        (if ok
            value
            (env-lookup (enclosing-env env) sym)))))

(defmethod env-set! ((env alist-env) sym value)
  (if (empty-env-p env)
      (error "~a undefined" sym)
      (multiple-value-bind (_ ok) (frame-lookup (first-frame env) sym)
        (declare (ignore _))
        (if ok
            (setf (cdr (assoc sym (first-frame env))) value)
            (env-set! (enclosing-env env) sym value)))))

(defmethod env-extend (params args (env alist-env))
  (make-instance
   'alist-env
   :frames
   (cons (mapcar #'cons params args)
         (env-frames env))))

(defmethod env-extend-with-bindings ((env alist-env) bindings)
  (env-extend (mapcar #'car bindings) (mapcar #'second bindings) env))

(defmethod env-extend-with-bindings* ((env alist-env) bindings)
  (destructuring-bind (bind . rest) bindings
    (let ((extended (env-extend (list (car bind)) (cdr bind) env)))
      (labels ((recur (binds)
                 (if (null binds)
                     extended
                     (destructuring-bind (f . r) binds
                       (env-define! extended (car f) (evaluate (second f) extended))
                       (recur r)))))
        (recur rest)))))

(defmethod env-copy ((env alist-env))
  (make-instance 'alist-env :frames (copy-list (env-frames env))))

(defun frame-variables (frame)
  (mapcar #'car frame))

(defun frame-values (frame)
  (mapcar #'cdr frame))

(defun frame-lookup (frame name)
  (let ((pair (assoc name frame :test #'string=)))
    (values (cdr pair) (not (null pair)))))

(defun bind-to-frame (frame var val)
  (cons (cons var val) frame))
