EMACS = emacs
EMACS_MAJOR_VERSION ::= `$(EMACS) -version | sed -n -e "s/.*GNU Emacs \([0-9][0-9]*\).*/\1/p"`

check: compile
	if [ $(EMACS_MAJOR_VERSION) -gt 23 ] ; then \
		$(EMACS) -q -batch -eval "(check-declare-file \"japanlaw.el\")" 2>&1 | grep -e "Checking" ; \
		$(EMACS) -q -batch -L . -l japanlaw.el -l japanlaw-test.el \
			-f ert-run-tests-batch-and-exit; \
		$(EMACS) -q -batch -L . -l japanlaw.elc -l japanlaw-test.el \
			-f ert-run-tests-batch-and-exit; \
	fi

compile:
	$(EMACS) --version
	$(EMACS) -q -batch -L . -f batch-byte-compile japanlaw.el
