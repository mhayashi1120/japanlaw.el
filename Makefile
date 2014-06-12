EMACS = emacs

check: compile
	$(EMACS) -q -batch -L ./revive -l japanlaw.el -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit
	$(EMACS) -q -batch -L ./revive  -l japanlaw.elc -l japanlaw-test.el \
		-f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -q -batch -L ./revive -f batch-byte-compile \
		japanlaw.el
