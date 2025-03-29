(asdf:defsystem :cl-scheme
  :description "A Scheme implementation in Common Lisp."
  :version "0.1.0"
  :author "Giorgos Papadokostakis"
  :depends-on ()
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "environment")
               (:file "evaluate")
               (:file "alist-environment")
               (:file "special-forms"))
  :in-order-to ((test-op (test-op :cl-scheme/test))))

(asdf:defsystem :cl-scheme/test
  :author "Giorgos Papadokostakis"
  :depends-on (:cl-scheme :rove)
  :pathname "t/"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "evaluate-test")
               (:file "special-forms-test"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
