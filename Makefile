
build:
	sbcl --noinform --eval "(asdf:initialize-source-registry \"$(PWD)\")" \
                    --eval "(asdf:load-system :cl-scheme)" \
                    --eval "(save-lisp-and-die \"scheme\" :toplevel #'cl-scheme:repl \
                                                          :executable t)"

run: build
	./scheme

test:
	sbcl --noinform --eval "(asdf:initialize-source-registry \"$(PWD)\")" \
                    --eval "(progn (print 'source-registry-parameter) (print asdf:*source-registry-parameter*))" \
                    --eval "(asdf:test-system :cl-scheme)" --quit

clean:
	rm scheme
