build:
	sbcl --noinform --eval "(asdf:initialize-source-registry '(:source-registry (:directory \"$(PWD)\") :inherit-configuration))" \
                    --eval "(asdf:load-system :cl-scheme)" \
                    --eval "(save-lisp-and-die \"scheme\" :toplevel #'cl-scheme:repl \
                                                          :executable t)"

run: build
	./scheme

clean:
	rm scheme
