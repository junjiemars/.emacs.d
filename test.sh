#!/bin/sh

_ROOT_="`cd $(dirname $0) && pwd`"
_EMACS_="${EMACS:-emacs}"
_TEST_="${_TEST_:-bone}"
_ENV_VER_=
_ENV_ERT_=

echo_env() {
  echo "------------"
  echo "VERSION: $_ENV_VER_"
  echo "ERT: $_ENV_ERT_"
  echo "TEST: $1"
  echo "------------"
}

test_bone() {
  echo_env "bone"
  ${_EMACS_} --batch                            \
             --no-init-file                     \
             --load="${_ROOT_}/init.el"
}

test_debug() {
  echo_env "debug"
  ${_EMACS_} --debug-init                       \
             --no-init-file                     \
             --load="${_ROOT_}/init.el"
}

test_axiom() {
  echo_env "axiom|clean"
  if [ "ert" = "$_ENV_ERT_" ]; then
    ${_EMACS_} --no-init-file                   \
               --chdir="${_ROOT_}"              \
               --load="${_ROOT_}/init.el"       \
               --eval="(clean-compiled-files)"
    echo_env "axiom|ert"
    # ${_EMACS_} --batch                                  \
    #            --no-site-file                           \
    #            --no-init-file                           \
    #            --load="${_ROOT_}/init.el"               \
    #            --load="ert"                             \
    #            --load="${_ROOT_}/test.el"               \
    #            --eval="(ert-run-tests-batch-and-exit)"
  else
    echo "#skipped axiom testing, ert no found"
  fi
}

# check env
_ENV_VER_=`${_EMACS_} --batch --eval="(prin1 emacs-version)"`
if [ ert = `${_EMACS_} --batch --eval="(prin1 (require 'ert nil t))"` ]; then
  _ENV_ERT_="ert"
fi

case "${_TEST_}" in
  bone)     test_bone     ;;
  axiom)    test_axiom    ;;
  debug)    test_debug    ;;
esac


# eof
