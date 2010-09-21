

MyBot:
	sbcl  --end-runtime-options --no-sysinit --no-userinit --disable-debugger \
	--load MyBot.lisp \
	--eval "(save-lisp-and-die \"MyBot\" :executable t :toplevel #'pwbot::main)"

submit: MyBot.lisp Makefile planet-wars.lisp split-sequence.lisp
	zip -r entry.zip MyBot.lisp Makefile planet-wars.lisp split-sequence.lisp
