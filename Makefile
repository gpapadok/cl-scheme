build:
	sbcl --noinform --eval "(asdf:load-system :cl-scheme)" \
                    --eval "(save-lisp-and-die \"scheme\" :toplevel #'cl-scheme:repl \
                                                           :executable t)"

run: build
	./scheme
