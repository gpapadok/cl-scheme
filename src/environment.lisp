(in-package #:cl-scheme)

;; Use cl built-ins for booleans
(eval-when (:compile-toplevel :load-toplevel)
  (set-dispatch-macro-character #\# #\t #'(lambda (stream subchar arg)
                                            (declare (ignore stream
                                                             subchar
                                                             arg))
                                            t))
  (set-dispatch-macro-character #\# #\f #'(lambda (stream subchar arg)
                                            (declare (ignore stream
                                                             subchar
                                                             arg))
                                            nil)))


(defgeneric env-copy (env)
  (:documentation "Creates a copy of the environment."))

(defgeneric env-push! (env name value)
  (:documentation "Binds a value to a name in the environment."))

(defgeneric env-lookup (env name)
  (:documentation "Returns the value bound to the name in the environment."))

(defgeneric env-update! (env name value)
  (:documentation "Updates a name with a new value in an environment."))

(defgeneric env-extend (params args env)
  (:documentation "Extends an environment by binding each param to its correspoding arg."))

(defgeneric env-extend-with-bindgins (env bindings)
  (:documentation "Extends an environment with the bindings (similar to a let form)."))

(defgeneric env-extend-with-bindings* (env bindings)
  (:documentation "Extends an environment with the bindings (similar to a let* form)."))

(defgeneric create-env (env-type)
  (:documentation "Creates a new empty environment.")
  (:method (type)
    nil))

(defgeneric create-global-env (env-type)
  (:documentation "Creates a new global environment with the standard library.")
  (:method (type)
    nil))
