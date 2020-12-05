#!/bin/sh

_ROOT_="`cd $(dirname $0) && pwd`"
_EMACS_="${EMACS:-emacs}"
_TEST_="${_TEST_:-bone}"

test_bone() {
  ${_EMACS_} --batch                              \
             --no-site-file                       \
             --load="${_ROOT_}/init.el"
}

test_debug() {
  ${_EMACS_} --debug-init                         \
             --no-site-file                       \
             --load="${_ROOT_}/init.el"
}

test_axiom() {
  local has_ert=`${_EMACS_} --batch --eval="(prin1 (require 'ert nil t))"`
  if [ "ert" = "$has_ert" ]; then
    ${_EMACS_} --batch                                  \
               --no-site-file                           \
               --load="${_ROOT_}/init.el"               \
               --load="ert"                             \
               --load="${_ROOT_}/test.el"               \
               --eval="(ert-run-tests-batch-and-exit)"
  else
    echo "#skipped axiom testing, ert no found"
  fi
}

case "${_TEST_}" in
  bone)     test_bone     ;;
  axiom)    test_axiom    ;;
  debug)    test_debug    ;;
esac


# eof
