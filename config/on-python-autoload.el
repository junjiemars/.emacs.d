;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-python.el
;;;;


(with-eval-after-load 'python
  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-p") #'run-python)))


;; (when-version% <= 24.1

;;   (defun python-virtualenv-activate (&optional vdir)
;; 		"Activate virtualenv at VDIR."
;;     (interactive "Dvirtualenv activate at ")
;;     (let ((vdir (or vdir default-directory)))
;;       (setq python-shell-process-environment
;;             (list
;;              (concat "PATH=" vdir path-separator (shell-env-> :path))
;;              (concat "VIRTUAL_ENV=" vdir)))
;;       (if-version%
;;           <= 25.1
;;           (setq python-shell-virtualenv-root vdir)
;;         (setq python-shell-virtualenv-path vdir)))))


;; (defun unload-python-on-exit ()
;;   (safe-fn-|when* 'elpy-disable (elpy-disable))
;;   (safe-fn-|when* 'pyvenv-deactivate (pyvenv-deactivate)))


;; (defadvice pyvenv-activate (after pyvenv-activate-after compile)
;;   (let ((p "autopep8 flake8 importmagic ipython jedi rope yapf"))
;;     (if (zerop
;;          (shell-command
;;           (concat "pip install " p
;;                   (if-platform%
;;                       'windows-nt " >/nul"
;;                     " >/dev/null"))))
;;         (message "#Install elpy required packages[%s]...done" p)
;;       (message "#Missing some packages[%s] that elpy required" p))))


;; (defadvice elpy-enable (after elpy-enable-after compile)
;;   (elpy-use-ipython))


;; (add-hook 'kill-emacs-hook #'unload-python-on-exit)
