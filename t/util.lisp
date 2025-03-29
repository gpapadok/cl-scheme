(in-package #:cl-scheme/test)

(defmacro ok-eval (sexp)
  `(ok (evaluate ',sexp *global-test-env*)))
