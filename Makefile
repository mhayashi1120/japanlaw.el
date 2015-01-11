EMACS = emacs

check: compile
	$(EMACS) -q -batch -l japanlaw.el -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -l japanlaw.elc -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -f batch-byte-compile \
		japanlaw.el
