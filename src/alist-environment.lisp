(in-package #:cl-scheme)

(defclass alist-env ()
  ((bindings
    :initarg :bindings
    :initform '(nil)
    :accessor bindings)))

(defmethod create-env ((env-type (eql :alist-env)))
  (make-instance 'alist-env))

(defmethod create-global-env ((env-type (eql :alist-env)))
  (let ((env (make-instance
              'alist-env
              :bindings
              (list
               nil
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
               (cons 'display #'princ)  ; TODO: It also prints output
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
               (cons 'evaluate #'evaluate)))))
    (load-script "src/scm/core.scm" env :quiet t)
    env))

(defmethod env-push! ((env alist-env) name value)
  (push-cdr! (cons name value) (bindings env)))

(defmethod env-lookup ((env alist-env) sym)
  (let ((variable (assoc sym (bindings env) :test #'string=)))
    (if (consp variable)
	    (cdr variable)
	    (error "~a undefined" sym))))

(defmethod env-update! ((env alist-env) sym value)
  (setf (cdr (assoc sym (bindings env))) value))

(defmethod env-extend (params args (env alist-env))
  (make-instance
   'alist-env
   :bindings
   (cons nil
         (append (loop
                   for sym in params
                   for val in args
                   collect (cons sym val))
                 (cdr (bindings env))))))

(defmethod env-extend-with-bindings ((env alist-env) bindings)
  (make-instance
   'alist-env
   :bindings
   (cons nil
         (append (loop
                   for bind in bindings
                   collect (cons (car bind)
                                 (evaluate (cadr bind) env)))
                 (cdr (bindings env))))))

(defmethod env-extend-with-bindings* ((env alist-env) bindings)
  (if-let (bind (car bindings))
    (env-extend-with-bindings*
     (make-instance
      'alist-env
      :bindings
      (append
       (list nil
             (cons (car bind)
                   (evaluate (cadr bind) env)))
       (cdr (bindings env))))
     (cdr bindings))
    env))

(defmethod env-copy ((env alist-env))
  (make-instance 'alist-env :bindings (copy-list (bindings env))))
