#!/usr/bin/env bash

_ROOT_="`cd $(dirname $0) && pwd`"
_EMACS_="${EMACS:-emacs}"
_TEST_="${_TEST_:-basic}"

test_basic() {
  ${_EMACS_} --batch                            \
             --no-site-file                     \
             --load="${_ROOT_}/init.el"         \
             --eval='(clean-compiled-files)'    \
             --load="${_ROOT_}/init.el"
}

test_debug() {
  ${_EMACS_} --debug-init                       \
             --no-site-file                     \
             --load="${_ROOT_}/init.el"         \
             --eval='(clean-compiled-files)'    \
             --load="${_ROOT_}/init.el"
}

case "${_TEST_}" in
  basic)    test_basic    ;;
  debug)    test_debug    ;;
esac

# ${_EMACS_} --batch                              \
#       --no-site-file                            \
#       --load="${_ROOT_}/init.el"                \
#       --eval='(clean-compiled-files)'           \
#       --load="${_ROOT_}/init.el"                \
#       --load="ert"                              \
#       --load="${_ROOT_}/test.el"                \
#       --eval="(ert-run-tests-batch-and-exit)"

