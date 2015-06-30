CASK ?= cask
EMACS ?= emacs

all:
	${MAKE} clean-elc
	${MAKE} unit
	${MAKE} build-strict
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner

test:
	${CASK} exec ${EMACS} --version
	${CASK} exec ${EMACS} -Q --batch -L . -l test/test-helper.el \
		-l test/r-like-example-test.el \
		-f ert-run-tests-batch-and-exit r-like-example.el

build :
	${CASK} exec $(EMACS) -Q --batch -L . --eval \
		"(progn \
		(batch-byte-compile))" r-like-example.el

build-strict:
	${CASK} exec ${EMACS} -Q --batch -L . --eval \
		"(progn \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))" r-like-example.el

clean-elc:
	rm -f *.elc
	rm -f test/*.elc

.PHONY: all unit test build build-strict clean-elc
