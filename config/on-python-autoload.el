;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
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


(defalias 'python*-program
  (lexical-let% ((b (or (and (executable-find% "python3") "python3")
                        "python")))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t b))))
  "Adjoint of `python-shell-interpreter'.")


(defun python*-version ()
  "Return version of `python*-program'."
  (let ((rc (shell-command* (python*-program) "--version")))
    (if (zerop (car rc))
        (match-string* "^Python \\([.0-9]+\\)$" (cdr rc) 1)
      (user-error* "!%s no found" (or (python*-program)
                                      "python*-program")))))


(defun python*-venv@ (&optional dir)
  "Activate virtualenv at DIR.

PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.

After Python3.3+, we can use `python -m venv <dir>' to create a
new virtual env at <dir>.

Using `sys.prefix', `sys.base_prefix' or `sys.real_prefix' to
determine whether inside a virtual env. Another way is using `pip
-V'."
  (interactive "Dvirtualenv activate at ")
  (let ((d (string-trim> (path! (expand-file-name (or dir
                                                      default-directory)))
                         "/")))
    (if (and (not (executable-find% "virtualenv"))
             (string< (python*-version) "3.3"))
        (user-error* "!virtualenv no found")
      (if (string< (python*-version) "3.3")
          (unless (file-exists-p (concat d "/bin/activate"))
            (let ((rc (shell-command* "virtualenv" d)))
              (unless (zerop (car rc))
                (user-error* "!%s" (string-trim> (cdr rc))))))
        (unless (file-exists-p (concat d "/bin/activate"))
          (let ((rc1 (shell-command* (python*-program) "-m" "venv" d)))
            (unless (zerop (car rc1))
              (user-error* "!%s" (string-trim> (cdr rc1)))))))
      (if-var% python-shell-virtualenv-root 'python
               (setq python-shell-virtualenv-root d)
        (if-var% python-shell-virtualenv-path 'python
                 (setq python-shell-virtualenv-path d)
          (if-var% python-shell-process-environment 'python
                   (setq python-shell-process-environment
                         (list
                          (concat "PYTHONPATH=" p)
                          (concat "PYTHONHOME=" d)
                          (concat "VIRTUAL_ENV=" d)))
            (setenv "PYTHONPATH" p)
            (setenv "PYTHONHOME" d)
            (setenv "VIRTUAL_ENV" d)))))))


;; (defun python*-inside-venv-p ()
;;   "Predicate inside venv."
;;   (python-shell-send-string "import sys\nprint(sys.prefix)\n"))


(with-eval-after-load 'python

  (when-var% python-shell-interpreter 'python
    (setq python-shell-interpreter (python*-program)))

  (when-var% python-shell-completion-native-enable 'python
    (setq python-shell-completion-native-enable
          (when-platform% 'gnu/linux t)))

  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-z") #'run-python)))


;; end of on-python-autoload.el
