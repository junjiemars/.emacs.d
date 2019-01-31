#!/bin/bash

ROOT="`cd $(dirname $0) && pwd`"
EMACS="${EMACS:-emacs}"

# emacs --batch \
# 			-l init.el \
# 			-l .travis-tests.el \
# 			-f ert-run-tests-batch-and-exit

emacs --batch \
			--load="${ROOT}/init.el" \
			--eval='(clean-compiled-files)'

emacs --batch \
			--load="${ROOT}/init.el"

