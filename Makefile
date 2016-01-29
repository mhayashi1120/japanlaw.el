EMACS = emacs

check: compile
	$(EMACS) -q -batch -L . -l japanlaw.el -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L . -l japanlaw.elc -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --version
	$(EMACS) -q -batch -L . -f batch-byte-compile japanlaw.el
