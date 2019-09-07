;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;


(defun python-virtualenv-activate (&optional dir)
  "Activate virtualenv at DIR.

PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path."
  (interactive "Dvirtualenv activate at ")
  (let ((d (string-trim> (or dir default-directory) "/")))
    (if-var% python-shell-virtualenv-root 'python
             (setq python-shell-virtualenv-root d)
      (let ((p (concat d path-separator
                       (alist-get* "PATH"
                                   (shell-env-> :env-vars)
                                   (getenv "PATH")
                                   nil
                                   #'string=))))
        (if-var% python-shell-process-environment 'python
                 (setq python-shell-process-environment
                       (list
                        (concat "PYTHONPATH=" p)
                        (concat "PYTHONHOME=" d)
                        (concat "VIRTUAL_ENV=" d)))
          (setenv "PYTHONPATH" p)
          (setenv "PYTHONHOME" d)
          (setenv "VIRTUAL_ENV" d))))))


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


(with-eval-after-load 'python

  (when-var% python-shell-completion-native-enable 'python
    (add-hook 'inferior-python-mode-hook
              #'(lambda ()
                  (setq python-shell-completion-native-enable
                        (python-shell-completion-native-setup)))))
  
  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-p") #'run-python)))
