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


(defun python*-version (&optional python)
  "Return the version of PYTHON"
  (let ((rc (shell-command* (or python (python*-program)) "--version")))
    (when (zerop (car rc))
      (match-string* "^Python \\([.0-9]+\\)$" (cdr rc) 1))))


(defun python*-activate-venv (&optional dir python)
  "Activate PYTHON's virtualenv at DIR.

PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.

After Python3.3+, we can use `python -m venv <dir>' to create a
new virtual env at <dir>.

Using `sys.prefix' to determine whether inside a virtual
env. Another way is using `pip -V'."
  (interactive "Dvirtualenv activate at ")
  (let* ((d (string-trim> (path! (expand-file-name dir))
                          "/"))
         (p (python*-program python))
         (v (python*-version python))
         (v3 (string< v "3.3")))
    (unless (file-exists-p (concat d "/bin/activate"))
      (cond ((and p v3 (executable-find% "virtualenv"))
             (let ((rc (shell-command* "virtualenv" "-p" p d)))
               (unless (zerop (car rc))
                 (user-error* "!%s" (string-trim> (cdr rc))))))
            ((and p (not v3))
             (let ((rc (shell-command* p "-m" "venv" d)))
               (unless (zerop (car rc))
                 (user-error* "!%s" (string-trim> (cdr rc))))))
            (t (user-error* "!python venv unavailable"))))
    (when-var% python-shell-interpreter 'python
      (setq python-shell-interpreter (python*-program)))
    (if-var% python-shell-virtualenv-root 'python
             (setq python-shell-virtualenv-root d)
      (if-var% python-shell-virtualenv-path 'python
               (setq python-shell-virtualenv-path d)
        (if-var% python-shell-process-environment 'python
                 (setq python-shell-process-environment
                       (list
                        (concat "PYTHONPATH=" d)
                        (concat "PYTHONHOME=" d)
                        (concat "VIRTUAL_ENV=" d)))
          (setenv "PYTHONPATH" d)
          (setenv "PYTHONHOME" d)
          (setenv "VIRTUAL_ENV" d))))))


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
