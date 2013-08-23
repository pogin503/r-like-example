UNAME := $(shell uname -s)

ifeq ($(UNAME),Darwin)
EMACS ?= /Applications/Emacs.app/Contents/MacOS/Emacs
else
EMACS ?= emacs
endif

export EMACS

CURL=curl --silent
WORK_DIR=$(shell pwd)

PACKAGE_NAME = $(shell basename $(WORK_DIR))
EMACS_BATCH = -Q --batch -L .

TEST_DIR := test

ERT=-l $(TEST_DIR)/lib/ert
TEST_DEP_1_STABLE_URL=http://bzr.savannah.gnu.org/lh/emacs/emacs-24/download/head:/ert.el-20110112160650-056hnl9qhpjvjicy-2/ert.el
TEST_DEP_1_LATEST_URL=https://raw.github.com/emacsmirror/emacs/master/lisp/emacs-lisp/ert.el

CASK ?= "${HOME}/.cask/bin/cask"
# export EMACS ?= emacs
export CASK

# PKG_DIR = $(shell ${CASK} package-directory)

# $(info cask $(CASK))
$(info pkg_dir $(PKG_DIR))
$(info work_dir $(WORK_DIR))
$(info package_name $(PACKAGE_NAME))
$(info path $(PATH))
$(warning test $(test))

.PHONY : build test downloads downloads-latest elpa clean

test: elpa build
	@echo "  start test"
	${CASK} exec ${EMACS} -Q --batch -l test/run-test.el \
		-f ert-run-tests-batch-and-exit
# --eval \
# "(progn (require 'cask \"~/.cask/cask.el\") \
# (cask-initialize))"
RPKG_DIR = ~/.emacs.d/plugins/r-like-example

elpa: ${PKG_DIR}
${PKG_DIR}: Cask
	@echo "  start elpa"
	${CASK} install
	touch $@
	@echo "  end elpa"

# touch $@elpa:
# 	cd $(RPKG_DIR)
# 	$(shell ${CASK} package-directory)
# 	${CASK} install

downloads :
	$(CURL) '$(TEST_DEP_1_STABLE_URL)' > $(TEST_DIR)/ert.el

downloads-latest :
	$(CURL) '$(TEST_DEP_1_LATEST_URL)' > $(TEST_DIR)/ert.el

build :
	@echo "  start build"
	$(EMACS) $(EMACS_BATCH) --eval \
		"(progn \
		(batch-byte-compile))" *.el
	@echo "  end build"

build-strict:
	@echo "  start build-strict"
	$(EMACS) $(EMACS_BATCH) $(ERT) --eval \
		"(progn \
		(setq byte-compile-error-on-warn t) \
		(batch-byte-compile))" *.el
	@echo "  end build-strict"


clean:
	rm -rf ${PKG_DIR}
