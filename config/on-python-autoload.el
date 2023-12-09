;; -*- lexical-binding:t -*-
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


(defun python*-version (python)
  "Return the version of PYTHON"
  (let ((rc (shell-command* python "--version")))
    (when (zerop (car rc))
      (string-match* "^Python \\([.0-9]+\\)$" (cdr rc) 1))))


(defalias 'python*-program
  (lexical-let% ((b (let ((p (or (executable-find% "python3")
                                 (executable-find% "python"))))
                      (when p (cons p (python*-version p))))))
    (lambda (&optional op n)
      "OP N"
      (cond ((eq :new op) (setq b (cons n (python*-version n))))
            ((eq :bin op) (car b))
            ((eq :ver op) (cdr b))
            (t b))))
  "Adjoint of \\=`python-shell-interpreter\\='.")


(defun python*-venv-activate! (&optional dir)
  "Activate Python's virtualenv at DIR.\n
PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.\n
After Python3.3+, we can use \\=`python -m venv DIR\\=' to create
 a new virtual env at DIR, then Using \\=`sys.prefix\\=' to
 determine whether inside a virtual env. Another way is using
 \\=`pip -V\\='."
  (interactive "Dvirtualenv activate at ")
  (let ((pv (python*-program)))
    (unless pv (user-error "%s" "python program unavailable"))
    (let ((d (string-trim> (path! (expand-file-name dir)) "/"))
          (p (file-name-base (car pv)))
          (v3 (string< (cdr pv) "3.3")))
      (unless (file-exists-p (concat d "/bin/activate"))
        (cond ((and p v3 (executable-find% "virtualenv"))
               (let ((rc (shell-command* "virtualenv" "-p" p d)))
                 (unless (zerop (car rc))
                   (user-error "%s" (string-trim> (cdr rc))))))
              ((and p (not v3))
               (let ((rc (shell-command* p "-m" "venv" d)))
                 (unless (zerop (car rc))
                   (user-error "%s" (string-trim> (cdr rc))))))
              (t (user-error "python venv unavailable"))))
      (when-var% python-shell-interpreter 'python
        (setq python-shell-interpreter p))
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
            (setenv "VIRTUAL_ENV" d)))))))


(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; interpreter
  (when-var% python-shell-interpreter 'python
    (setq python-shell-interpreter (python*-program)))
  ;; completion
  (when-var% python-shell-completion-native-enable 'python
    (setq python-shell-completion-native-enable
          (when-platform% 'gnu/linux t)))
  ;; keys
  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-z") #'run-python)))


;;; `python' after load
(eval-after-load 'python #'on-python-init!)


;; end of on-python-autoload.el
