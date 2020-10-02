#!/usr/bin/env bash

ROOT="`cd $(dirname $0) && pwd`"
EMACS="${EMACS:-emacs}"

emacs --batch                                     \
      --no-site-file                              \
      --load="${ROOT}/init.el"                    \
      --eval='(clean-compiled-files)'             \
      --load="${ROOT}/init.el"

# emacs --batch                                   \
#       --no-site-file                            \
#       --load="${ROOT}/init.el"                  \
#       --eval='(clean-compiled-files)'           \
#       --load="${ROOT}/init.el"                  \
#       --load="ert"                              \
#       --load="${ROOT}/test.el"                  \
#       --eval="(ert-run-tests-batch-and-exit)"

