;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

;;; Python help instructions
;; help:    help(help)
;; dir:     list of strings, dir(sys.prefix)
;; inspect: inspect living objects, needs import
;; dis:     disassembler
;;;

(defun python-virtualenv-activate (&optional dir)
  "Activate virtualenv at DIR.

PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.

Outside a virtualenv, sys.real_prefix should not exist."
  (interactive "Dvirtualenv activate at ")
  (let ((d (string-trim> (expand-file-name (or dir
                                               default-directory))
                         "/")))
    (if-var% python-shell-virtualenv-root 'python
             (setq python-shell-virtualenv-root d)
      (if-var% python-shell-virtualenv-path 'python
               (setq python-shell-virtualenv-path d)
        (let ((p (concat d path-separator
                         (or (cdr (assoc** "PATH"
                                           (shell-env-> :env-vars)
                                           #'string=))
                             (getenv "PATH")))))
          (if-var% python-shell-process-environment 'python
                   (setq python-shell-process-environment
                         (list
                          (concat "PYTHONPATH=" p)
                          (concat "PYTHONHOME=" d)
                          (concat "VIRTUAL_ENV=" d)))
            (setenv "PYTHONPATH" p)
            (setenv "PYTHONHOME" d)
            (setenv "VIRTUAL_ENV" d)))))))


(with-eval-after-load 'python

  (when-var% python-shell-completion-native-enable 'python
    (setq python-shell-completion-native-enable
          (when-platform% 'gnu/linux t))
    (comment
     ;; `python-shell-completion-native-try' has bugs in Emacs 27+
     (when% (executable-find% python-shell-interpreter)
       (setq python-shell-completion-native-enable
             (eval-when-compile
               (require 'python)
               (run-python)
               (let ((p (get-buffer "*Python*")))
                 (when p
                   (with-current-buffer p
                     (prog1 (python-shell-completion-native-try)
                       (let ((kill-buffer-query-functions nil))
                         (kill-buffer p)))))))))))

  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-p") #'run-python)))


;; end of on-python-autoload.el
