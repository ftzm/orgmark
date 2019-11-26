LISP ?= sbcl

build:
	$(LISP) --userinit .sbclrc \
		--load orgmark.asd \
		--eval '(ql:quickload :orgmark)' \
		--eval '(asdf:make :orgmark)' \
		--eval '(quit)'
