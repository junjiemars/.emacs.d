#!/bin/sh

_ROOT_="${_ROOT_:-`cd -- $(dirname -- $0) && pwd`}"
_EMACS_="${_EMACS_:-emacs}"
_TEST_="${_TEST_:-bone}"
_WINNT_="${_WINNT_:-no}"
_ENV_HOM_="$_ROOT_"
_ENV_PRO_="private/self-prologue.el"
_ENV_VER_=
_ENV_ERT_=
_ENV_PKG_=

echo_env() {
  echo "------------"
  echo "VERSION: $_ENV_VER_"
  echo "TEST: $1"
  echo "------------"
}

make_env() {
  local d="`dirname ${_ROOT_}/${_ENV_PRO_}`"
  if [ -d "${_ROOT_}/${_ENV_PRO_}" ]; then
    return 0
  else
    mkdir -p "$d"
  fi
}

restore_env() {
  if [ -f "${_ROOT_}/${_ENV_PRO_}" ]; then
    rm "${_ROOT_}/${_ENV_PRO_}"
  else
    return 0
  fi
}

test_bone() {
  echo_env "bone|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (clean-compiled-files))"

  echo_env "bone|compile"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"

 echo_env "bone|boot"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"
}

test_debug() {
  echo_env "debug|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (clean-compiled-files))"

  echo_env "debug|capture"
  ${_EMACS_} --debug-init                                       \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (setq debug-on-error t)
  (load \"${_ENV_HOM_}/init.el\")
  (load (emacs-home* \"init.el\")))"
}

test_axiom() {
  if [ "ert" != "$_ENV_ERT_" ]; then
    echo "#skipped axiom testing, ert no found"
    return 0
  fi

  cat <<END > "${_ROOT_}/${_ENV_PRO_}"
(*self-paths* :put :env-spec nil)
(*self-paths* :put :package-spec nil)
(*self-paths* :put :epilogue nil)
END

  echo "# cat < ${_ROOT_}/${_ENV_PRO_}"
  cat < ${_ROOT_}/${_ENV_PRO_}

  echo_env "axiom|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (clean-compiled-files))"

  echo_env "axiom|compile"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (load (emacs-home* \"test.el\"))
  (ert-run-tests-batch-and-exit))"

  echo_env "axiom|boot"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (load (emacs-home* \"test.el\"))
  (ert-run-tests-batch-and-exit))"
}

test_package() {
  if [ "package" != "$_ENV_PKG_" ]; then
    echo "#skipped package testing, package no support"
    return 0
  fi

  cat <<END > "${_ROOT_}/${_ENV_PRO_}"
(*self-paths* :put :package-spec nil)
(*self-paths* :put :env-spec nil)
(*self-paths* :put :epilogue nil)
(*self-env-spec*
 :put :package
 (list :remove-unused nil
       :package-check-signature 'allow-unsigned
       :allowed t))
END

  echo "# cat < ${_ROOT_}/${_ENV_PRO_}"
  cat < "${_ROOT_}/${_ENV_PRO_}"

  echo_env "package|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (clean-compiled-files))"

  echo_env "package|compile"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"

  echo_env "package|boot"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"
}

test_extra() {
  if [ "package" != "$_ENV_PKG_" ]; then
    echo "#skipped package testing, package no support"
    return 0
  fi

  cat <<END > "${_ROOT_}/${_ENV_PRO_}"
(*self-paths* :put :package-spec nil)
(*self-paths* :put :env-spec nil)
(*self-paths* :put :epilogue nil)
(*self-env-spec*
 :put :package
 (list :remove-unused nil
       :package-check-signature 'allow-unsigned
       :allowed t))

(*self-packages*
 :put :scheme
 (list
  :cond (and (when-version% <= 23.2 t)
             ;; Nore Emacs has builtin supports for Chez
             ;; scheme and gambitC scheme, and does not need to
             ;; install the dumb geiser.
             (or (executable-find% "racket")
                 (executable-find% "scheme")
                 (executable-find% "chicken")
                 (executable-find% "guile")))
  :packages  '(geiser)
  :compile \`(,(compile-unit% (emacs-home* "config/use-geiser-autoload.el")))))

(*self-packages*
 :put :common-lisp
 (list
  :cond (or (executable-find% "sbcl")
            (executable-find% "ecl")
            (executable-find% "acl"))
  :packages '(slime)
  :compile \`(,(compile-unit% (emacs-home* "config/use-slime-autoload.el")))))
END

  echo "# cat < ${_ROOT_}/${_ENV_PRO_}"
  cat < "${_ROOT_}/${_ENV_PRO_}"

  echo_env "extra|clean"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\")
  (clean-compiled-files))"

  echo_env "extra|compile"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"

  echo_env "extra|boot"
  ${_EMACS_} --batch                                            \
             --no-window-system                                 \
             --eval="
(let ((user-emacs-directory \"${_ENV_HOM_}/\"))
  (load \"${_ENV_HOM_}/init.el\"))"
}

# check env
_ENV_VER_="`$_EMACS_ --batch --eval='(prin1 emacs-version)'`"
_ENV_ERT_="`$_EMACS_ --batch --eval='(prin1 (require (quote ert) nil t))'`"
_ENV_PKG_="`$_EMACS_ --batch --eval='(prin1 (require (quote package) nil t))'`"

if [ "$_WINNT_" = "yes" ]; then
  _ENV_HOM_="`echo $_ENV_HOM_ | sed -e 's#/\([a-z]\)/\(.*\)$#\1:/\2#g'`"
fi


# make env
make_env

# test
case "${_TEST_}" in
  bone)     test_bone     ;;
  axiom)    test_axiom    ;;
  package)  test_package  ;;
  extra)    test_extra    ;;
  debug)    test_debug    ;;
esac

# restore env
restore_env


# eof
