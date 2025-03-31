@echo off
rem check `cl' environment
rem required by `(emacs-home* "config/cc.el")'
rem from `(nore-emacs)'
pushd %%cd%%
cd /d "%s"
call vcvarsall.bat %s
set CC=cl
popd
echo "%%INCLUDE%%"
