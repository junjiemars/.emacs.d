;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; pythons.el
;;;;

;;; Python help instructions
;; help:    help(help)
;; dir:     list of strings, dir(sys.prefix)
;; inspect: inspect living objects, needs import
;; dis:     disassembler
;;;


(defmacro python*-version (python)
  "Return the version of PYTHON"
  `(let ((rc (shell-command* ,python "--version")))
     (when (zerop (car rc))
       (string-match* "^Python \\([.0-9]+\\)$" (cdr rc) 1))))


(defalias 'python*-program
  (lexical-let% ((b (eval-when-compile
                      (let ((p (or (executable-find% "python3")
                                   (executable-find% "python"))))
                        (when p (cons p (python*-version p)))))))
    (lambda (&optional op n)
      (cond ((eq :new op) (setq b (cons n (python*-version n))))
            ((eq :bin op) (car b))
            ((eq :ver op) (cdr b))
            (t b))))
  "Adjoint of \\=`python-shell-interpreter\\='.")


(defvar *python-venv-root* nil
  "Python venv root.")

(defun python*-venv-activate! (&optional dir)
  "Activate Python\\='s virtualenv at DIR.\n
PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.\n
After Python3.3+, we can use \\=`python -m venv DIR\\=' to create
 a new virtual env at DIR, then Using \\=`sys.prefix\\=' to
 determine whether inside a virtual env. Another way is using
 \\=`pip -V\\='."
  (interactive "Dpython venv activate at ")
  (let ((pv (python*-program)))
    (unless pv (user-error "%s" "python program unavailable"))
    (let ((d (string-trim> (path! (expand-file-name dir)) "/"))
          (p (file-name-base* (car pv)))
          (v3.3- (= -1 (version-string= (cdr pv) "3.3"))))
      (unless (file-exists-p (concat d "/bin/activate"))
        (cond ((and p v3.3- (executable-find% "virtualenv"))
               (let ((rc (shell-command* "virtualenv" "-p" p d)))
                 (unless (zerop (car rc))
                   (user-error "%s" (string-trim> (cdr rc))))))
              ((and p (not v3.3-))
               (let ((rc (shell-command* p "-m" "venv" d)))
                 (unless (zerop (car rc))
                   (user-error "%s" (string-trim> (cdr rc))))))
              (t (user-error "python venv unavailable"))))
      (when-var% python-shell-interpreter 'python
        (setq python-shell-interpreter p))
      (if-var% python-shell-virtualenv-root 'python
               (setq python-shell-virtualenv-root d
                     *python-venv-root* d)
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

(unless-platform% 'windows-nt
  (defalias 'python*-pip-mirror!
    (lexical-let*%
        ((b '("https://pypi.tuna.tsinghua.edu.cn/simple/"
              "https://pypi.mirrors.ustc.edu.cn/simple/"
              "http://pypi.hustunique.com/"
              "http://pypi.sdutlinux.org/")))
      (lambda (&optional op n)
        (unless *python-venv-root*
          (user-error "%s" "python venv unavailable"))
        (cond ((eq op :set)
               (shell-command* "source"
                 (concat *python-venv-root* "/bin/activate")
                 "&& pip config set global.index-url"
                 (nth (% n (length b)) b)))
              ((eq op :ls) (if (< n 0) b (nth (% n (length b)) b)))
              (t (let ((rc (shell-command* "source"
                             (concat *python-venv-root* "/bin/activate")
                             "&& pip config get global.index-url")))
                   (when (zerop (car rc))
                     (string-trim> (cdr rc))))))))
    "Python pip mirror."))

(unless-platform% 'windows-nt
  (defun python*-lsp-make! ()
    "Make pylsp.sh for \\=`elgot\\='."
    (interactive)
    (unless *python-venv-root*
      (user-error "%s" "python venv unavailable"))
    (let* ((pylsp (v-home% ".exec/pylsp.sh"))
           (rc (shell-command*
                   "chmod" "u+x"
                   (save-str-to-file
                    (concat
                     "#!/bin/sh\n"
                     "source " *python-venv-root* "/bin/activate\n"
                     "if ! pip show python-lsp-server &>/dev/null; then\n"
                     "  pip install python-lsp-server\n"
                     "fi\n"
                     "exec pylsp $@\n")
                    pylsp))))
      (when (zerop (car rc))
        (when-feature-eglot%
          (when-var% eglot-command-history 'eglot
            (push! pylsp eglot-command-history t)))))))



(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; interpreter
  (setq% python-shell-interpreter (python*-program :bin) 'python)
  ;; completion
  (setq% python-shell-completion-native-enable
         (when-platform% 'gnu/linux t) 'python)
  ;; keys
  (when-var% python-mode-map 'python
    ;; on ancient Emacs `(kbd "C-c C-p")' bind to
    ;; `python-previous-statement'
    (define-key% python-mode-map (kbd "C-c C-z") #'run-python)))



(provide 'pythons)

;; end of pythons.el
