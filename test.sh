#!/bin/sh

_ROOT_="${_ROOT_:-`cd $(dirname $0) && pwd`}"
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
  echo_env "bone|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="                                           \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (clean-compiled-files))                                       \
"

  echo_env "bone|compile"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="                                           \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (load (expand-file-name \"${_ROOT_}/init.el\")))              \
"

 echo_env "bone|boot"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="                                           \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (load (expand-file-name \"${_ROOT_}/init.el\")))              \
"
}

test_debug() {
  echo_env "debug|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="                                           \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (clean-compiled-files))                                       \
"

  echo_env "debug|capture"
  ${_EMACS_} --debug-init                                       \
             --eval="                                           \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (setq debug-on-error t)                                       \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (load (emacs-home* \"init.el\")))                             \
"
}

test_axiom() {
  if [ "ert" = "$_ENV_ERT_" ]; then
    echo_env "axiom|clean"
    ${_EMACS_} --batch                                          \
               --no-window-system                               \
               --eval="                                         \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\"))) \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (clean-compiled-files))                                       \
"
    echo_env "axiom|compile"
    ${_EMACS_} --batch                                          \
               --no-window-system                               \
               --eval="                                         \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\")))  \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (load (emacs-home* \"test.el\"))                              \
  (ert-run-tests-batch-and-exit))                               \
"

    echo_env "axiom|boot"
    ${_EMACS_} --batch                                          \
               --no-window-system                               \
               --eval="                                         \
(let ((user-emacs-directory (expand-file-name \"${_ROOT_}/\")))  \
  (load (expand-file-name \"${_ROOT_}/init.el\"))               \
  (load (emacs-home* \"test.el\"))                              \
  (ert-run-tests-batch-and-exit))                               \
"
  else
    echo "#skipped axiom testing, ert no found"
  fi
}

# check env
_ENV_VER_="`$_EMACS_ --batch --eval='(prin1 emacs-version)'`"
_ENV_ERT_="`$_EMACS_ --batch --eval='(prin1 (require (quote ert) nil t))'`"
if [ "ert" != "$_ENV_ERT_" ]; then
  _ENV_ERT_=
fi

case "${_TEST_}" in
  bone)     test_bone     ;;
  axiom)    test_axiom    ;;
  debug)    test_debug    ;;
esac


# eof
