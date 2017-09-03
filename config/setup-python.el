;;;;
;; Python
;;;;



(comment
 (defun set-python-mode! ()
   ;; Enable paredit for Python
   (enable-paredit-mode)
   ;; Working with camel-case tokens
   (subword-mode))
 (add-hook 'python-mode-hook #'set-python-mode!))


(defadvice pyvenv-activate (after pyvenv-activate-after compile)
  (let ((p "autopep8 flake8 importmagic ipython jedi rope yapf"))
    (if (zerop
         (shell-command (concat "pip install " p " >/dev/null")))
        (message "#Install elpy required packages[%s]...done" p)
      (message "#Missing some packages[%s] that elpy required" p))))


(defadvice elpy-enable (after elpy-enable-after compile)
  (elpy-use-ipython)
  (add-hook 'kill-emacs-hook #'pyvenv-deactivate)
  (add-hook 'kill-emacs-hook #'elpy-disable))


