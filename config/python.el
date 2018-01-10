;;;; -*- lexical-binding:t -*-
;;;;
;; Python environment
;;;;



(eval-when-compile (require 'python)) 



(defun python-virtualenv-activate (&optional vdir)
  "Activate virtualenv at VDIR"
  (interactive "Dvirtualenv activate at ")
  (let ((vdir (or vdir default-directory)))
    (setq python-shell-process-environment
          (list
           (concat "PATH=" vdir path-separator (shell-env-> :path))
           (concat "VIRTUAL_ENV=" vdir))
          python-shell-virtualenv-root vdir)))

;; (defun unload-python-on-exit ()
;;   (safe-fn-when* 'elpy-disable (elpy-disable))
;;   (safe-fn-when* 'pyvenv-deactivate (pyvenv-deactivate)))


;; (defadvice pyvenv-activate (after pyvenv-activate-after compile)
;;   (let ((p "autopep8 flake8 importmagic ipython jedi rope yapf"))
;;     (if (zerop
;;          (shell-command
;;           (concat "pip install " p
;;                   (platform-supported-if
;;                       windows-nt " >/nul"
;;                     " >/dev/null"))))
;;         (message "#Install elpy required packages[%s]...done" p)
;;       (message "#Missing some packages[%s] that elpy required" p))))


;; (defadvice elpy-enable (after elpy-enable-after compile)
;;   (elpy-use-ipython))


;; (add-hook 'kill-emacs-hook #'unload-python-on-exit)


