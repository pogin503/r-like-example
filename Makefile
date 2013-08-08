UNAME:=$(shell uname -s)

ifeq ($(UNAME),Darwin)
EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
else
EMACS=emacs
endif

CURL=curl --silent
WORK_DIR=$(shell pwd)

PACKAGE_NAME=$(shell basename $(WORK_DIR))
EMACS_BATCH=-Q --batch -L .

$(info work_dir $(WORK_DIR))
$(info package_name $(PACKAGE_NAME))

TEST_DIR=test

# LIB= -l $(TEST_DIR)/lib/cl-lib
ERT=-l $(TEST_DIR)/lib/ert
TEST_DEP_1_STABLE_URL=http://bzr.savannah.gnu.org/lh/emacs/emacs-24/download/head:/ert.el-20110112160650-056hnl9qhpjvjicy-2/ert.el
TEST_DEP_1_LATEST_URL=https://raw.github.com/emacsmirror/emacs/master/lisp/emacs-lisp/ert.el

CASK ?= "${HOME}/.cask/bin/cask"
# export EMACS ?= emacs

export EMACS
export CASK

PKG_DIR := $(shell ${CASK} package-directory)

.PHONY : build test downloads downloads-latest elpa clean

emacs-version:
	${EMACS} --version

$(info cask $(CASK))
$(info pkg_dir $(PKG_DIR))
$(info path $(PATH))
$(warning test- $(test))

test: elpa emacs-version build
	${CASK} exec ${EMACS} -Q --batch --load test/run-test.el \
		-f ert-run-tests-batch-and-exit
# --eval \
# "(progn (require 'cask \"~/.cask/cask.el\") \
# (cask-initialize))"
RPKG_DIR = ~/.emacs.d/plugins/r-like-example/Cask

elpa: $(RPKG_DIR)
	${CASK} install
	touch $@

downloads :
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DIR)/ert.el

downloads-latest :
	$(CURL) '$(TEST_DEP_1_LATEST_URL)' > $(TEST_DIR)/ert.el

build :
	$(EMACS) $(EMACS_BATCH) $(ERT) --eval \
		"(progn \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))"
# clean:
# 	rm -rf ${PKG_DIR}

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l test/run-test.el
