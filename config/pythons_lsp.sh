#!/bin/sh
# launch `pylsp'
# required by `(emacs-home* "config/pythons.el")'
# from `(nore-emacs)'
. %sbin/activate
if ! pip -qqq show python-lsp-server; then
  pip install python-lsp-server python-lsp-ruff pyflakes isort
fi
exec %sbin/pylsp $@
