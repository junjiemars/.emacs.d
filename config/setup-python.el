;;;;
;; Python
;;;;




(defun unload-python-on-exit ()
  (safe-fn-when* 'elpy-disable (elpy-disable))
  (safe-fn-when* 'pyvenv-deactivate (pyvenv-deactivate)))


(defun install-elpy-requirements ()
  (let ((p "autopep8 flake8 importmagic ipython jedi rope yapf"))
    (if (zerop
         (shell-command (concat "pip install " p " >/dev/null")))
        (message "#Install elpy required packages[%s]...done" p)
      (message "#Missing some packages[%s] that elpy required" p))))


(defadvice elpy-enable (after elpy-enable-after compile)
  (elpy-use-ipython))


(add-hook 'kill-emacs-hook #'unload-python-on-exit)
(add-hook 'pyvenv-post-activate-hooks #'install-elpy-requirements)

