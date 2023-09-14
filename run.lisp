(in-package :cl-user)

(load "scheme.lisp")

(defconstant +command-line-args+
  (or #+SBCL (cdr *posix-argv*)
      nil))

(if (>= (length +command-line-args+) 1)
      (scm:load-script (car +command-line-args+))
      (scm:repl))
