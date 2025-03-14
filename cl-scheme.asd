(asdf:defsystem :cl-scheme
  :description "A Scheme implementation in Common Lisp."
  :version "0.1.0"
  :author "Giorgos Papadokostakis"
  :depends-on (:arrow-macros)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "special-forms")
               (:file "scheme")
               ;; (:file "run")
               )
  :in-order-to ((test-op (test-op :cl-scheme/test))))

(asdf:defsystem :cl-scheme/test
  :author "Giorgos Papadokostakis"
  :depends-on (:cl-scheme :rove)
  :pathname "t/"
  :components ((:file "scheme-test"))
  :perform (test-op (o c) (symbol-call :rove :run c)))
