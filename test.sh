#!/bin/sh

_ROOT_="${_ROOT_:-$(cd -- $(dirname -- $0) && pwd)}"
_EMACS_="${_EMACS_:-emacs}"
_TEST_="${_TEST_}"
_WINNT_="${_WINNT_:-no}"
_ENV_PRO_="${_ROOT_}/private/self-prologue.el"
_ENV_VER_=
_ENV_ERT_=
_ENV_PKG_=

test_echo_env() {
  echo "# ------------"
  echo "# VERSION: $_ENV_VER_"
  echo "# TEST: $1"
  echo "# ------------"
}

test_clean_env() {
  cat <<END >"${_ENV_PRO_}"
(*self-paths* :put :env-spec nil)
(*self-paths* :put :package-spec nil)
(*self-paths* :put :epilogue nil)
END
  echo "# cat <${_ENV_PRO_}"
  cat <"${_ENV_PRO_}"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (defvar *nore-emacs-no-boot* nil)
  (load \"${_ROOT_}/init.el\")
  (clean-compiled-files))
"
}

test_bone() {
  test_echo_env "bone|clean"
  test_clean_env
  test_echo_env "bone|compile"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="(load \"${_ROOT_}/init.el\")"
  test_echo_env "bone|boot"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (load \"${_ROOT_}/init.el\")
  (message \"Elapsed: %s\" (emacs-init-time)))
"
}

test_axiom() {
  test_echo_env "axiom|check"
  if [ "ert" != "$_ENV_ERT_" ]; then
    echo "# skip axiom testing ..., ert no found"
    return 0
  else
    echo "_ENV_ERT_: ${_ENV_ERT_}"
  fi
  test_echo_env "axiom|clean"
  test_clean_env
  test_echo_env "axiom|compile"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (load \"${_ROOT_}/init.el\")
  (load (emacs-home* \"test.el\"))
  (ert-run-tests-batch-and-exit))
"
  test_echo_env "axiom|boot"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (load \"${_ROOT_}/init.el\")
  (load (emacs-home* \"test.el\"))
  (ert-run-tests-batch-and-exit))
"
}

test_package() {
  test_echo_env "package|check"
  if [ "package" != "$_ENV_PKG_" ]; then
    echo "# skipped package testing, package no support"
    return 0
  else
    echo "_ENV_PKG_: ${_ENV_PKG_}"
  fi
  test_echo_env "package|clean"
  test_clean_env
  test_echo_env "package|prologue"
  cat <<END > "${_ENV_PRO_}"
(*self-paths* :put :package-spec nil)
(*self-paths* :put :env-spec nil)
(*self-paths* :put :epilogue nil)
(*self-env-spec*
  :put :package
  (list :remove-unused t
        :package-check-signature 'allow-unsigned
        :allowed t))
(*self-packages*
  :put :lisp
  (list
   :cond (when-version% <= 29 t)
   :packages  '(paredit magit)
   :compile \`(,(compile-unit% (emacs-home* "config/use-lisp-autoload.el"))
               ,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))
END
  echo "# cat <${_ENV_PRO_}"
  cat <"${_ENV_PRO_}"
  test_echo_env "package|compile"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="(load \"${_ROOT_}/init.el\")"
  test_echo_env "package|boot"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (load \"${_ROOT_}/init.el\")
  (message \"Elapsed: %s\" (emacs-init-time)))
"
}

test_profile() {
	local d="${_ROOT_}/.profile"
	local fc="${d}/compile"
	local fb="${d}/boot"
	local prof="${_ROOT_}/.github/prof.sh"
  test_echo_env "profile|clean"
  test_clean_env
  cat <<END > "${_ENV_PRO_}"
(*self-paths* :put :package-spec nil)
(*self-paths* :put :env-spec nil)
(*self-paths* :put :epilogue nil)
(*self-env-spec*
 :put :shell
 (list :copy-vars \`("PATH")
       :spin-vars \`(("ZZZ" . "123"))
       :exec-path t
       :shell-file-name (or (executable-find% "zsh")
                            (executable-find% "bash"))
       :allowed t))
(*self-env-spec*
  :put :package
  (list :package-check-signature 'allow-unsigned
        :allowed t))
(*self-packages*
  :put :lisp
  (list
   :cond (when-version% <= 29 t)
   :packages  '(paredit magit)
   :compile \`(,(compile-unit% (emacs-home* "config/use-lisp-autoload.el"))
               ,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))
END
  echo "# cat <${_ENV_PRO_}"
  cat <"${_ENV_PRO_}"
	mkdir -p "$d"
  test_echo_env "profile|compile"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (defvar *nore-emacs-profile* nil)
  (load \"${_ROOT_}/init.el\"))" 2>"$fc"
	$prof "$fc"
  test_echo_env "profile|boot"
  ${_EMACS_} --batch \
             --no-window-system \
             --eval="
(progn
  (defvar *nore-emacs-profile* nil)
  (load \"${_ROOT_}/init.el\"))" 2>"$fb"
	$prof "$fb"
}

test_debug() {
  test_echo_env "debug|clean"
  test_clean_env
  test_echo_env "debug|compile"
  ${_EMACS_} --batch \
             --no-window-system \
             --debug-init \
             --eval="
(progn
  (setq debug-on-error t)
  (load \"${_ROOT_}/init.el\"))"
}

check_env() {
  echo "# check env ..."
  echo "# _ROOT_: ${_ROOT_}"
  _ENV_VER_=$(${_EMACS_} --batch -Q --eval='(prin1 emacs-version)' \
                | sed 's/\"//g' \
                | cut -d '.' -f1,2)
  echo "# _ENV_VER_: ${_ENV_VER_}"
  _ENV_ERT_=$($_EMACS_ --batch --eval='(prin1 (require (quote ert) nil t))')
  echo "# _ENV_ERT_: ${_ENV_ERT_}"
  _ENV_PKG_=$($_EMACS_ --batch --eval='(prin1 (require (quote package) nil t))')
  echo "# _ENV_PKG_: ${_ENV_PKG_}"
}

make_env() {
  echo "# make env ..."
  export EMACS_HOME="${_ROOT_}/"
  echo "# EMACS_HOME: ${EMACS_HOME}"
}

restore_env() {
  echo "# restore env ..."
  if [ -f "${_ENV_PRO_}" ]; then
    rm "${_ENV_PRO_}"
  else
    return 0
  fi
  unset EMACS_HOME
}

check_env
make_env

# test
case "${_TEST_}" in
  bone)     test_bone     ;;
  axiom)    test_axiom    ;;
  package)  test_package  ;;
	profile)  test_profile  ;;
  debug)    test_debug    ;;
  *) ;;
esac

restore_env

# eof
