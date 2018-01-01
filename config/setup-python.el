;;;; -*- lexical-binding:t -*-
;;;;
;; Python
;;;;


;; Requirements:
;; `virtualenv'
;; `pip'
;;
;; How to play
;; 1. `M-x pyvenv' to activate virtual environment
;; 2. `M-x elpy-enable'
;; 3. open `*.py' file, `C-c C-c'


(defun unload-python-on-exit ()
  (safe-fn-when* 'elpy-disable (elpy-disable))
  (safe-fn-when* 'pyvenv-deactivate (pyvenv-deactivate)))


(defadvice pyvenv-activate (after pyvenv-activate-after compile)
  (let ((p "autopep8 flake8 importmagic ipython jedi rope yapf"))
    (if (zerop
         (shell-command
          (concat "pip install " p
                  (platform-supported-if
                      windows-nt " >/nul"
                    " >/dev/null"))))
        (message "#Install elpy required packages[%s]...done" p)
      (message "#Missing some packages[%s] that elpy required" p))))


(defadvice elpy-enable (after elpy-enable-after compile)
  (elpy-use-ipython))


(add-hook 'kill-emacs-hook #'unload-python-on-exit)


