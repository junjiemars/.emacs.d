;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; pythons.el
;;;;

;;;
;; Python help instructions
;; help:    help(help)
;; dir:     list of strings, dir(sys.prefix)
;; inspect: inspect living objects, needs import
;; dis:     disassembler
;;;

;;; version/program

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

;; end of version/program

;;;
;; venv
;;;

(unless-platform% 'windows-nt
  (defalias 'python*-venv-activate!
    (lexical-let%
        ((b (emacs-home* "private/progs/py/ve/")))
      (lambda (&optional dir)
        (let ((pv (python*-program)))
          (unless pv
            (user-error "%s" "python program unavailable"))
          (let ((d (path! (path+ (expand-file-name (or dir b)))))
                (p (car pv))
                (v- (= -1 (version-string= (cdr pv) "3.3"))))
            (unless (file-exists-p (concat d "/bin/activate"))
              (cond ((and p v- (executable-find% "virtualenv"))
                     (let ((rc (shell-command* "virtualenv" "-p" p d)))
                       (unless (zerop (car rc))
                         (user-error "%s" (string-trim> (cdr rc))))))
                    ((and p (not v-))
                     (let ((rc (shell-command* p "-m" "venv" d)))
                       (unless (zerop (car rc))
                         (user-error "%s" (string-trim> (cdr rc))))))
                    (t (user-error "%s" "python venv unavailable"))))
            (prog1 d
              (when-var% python-shell-interpreter 'python
                (setq python-shell-interpreter (concat d "bin/python")))
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
                    (setenv "VIRTUAL_ENV" d)))))))))
    "Activate Python\\='s virtualenv at DIR.\n
PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.\n
After Python3.3+, we can use \\=`python -m venv DIR\\=' to create
 a new virtual env at DIR, then Using \\=`sys.prefix\\=' to
 determine whether inside a virtual env. Another way is using
 \\=`pip -V\\='."))

;; end of venv

;;;
;; lsp
;;;

(unless-platform% 'windows-nt
  (defalias 'python*-pip-mirror!
    (lexical-let*%
        ((b '("https://pypi.tuna.tsinghua.edu.cn/simple/"
              "https://pypi.mirrors.ustc.edu.cn/simple/"
              "http://pypi.hustunique.com/"
              "http://pypi.sdutlinux.org/")))
      (lambda (&optional op n)
        (let ((venv (python*-venv-activate!)))
          (unless venv
            (user-error "%s" "python venv unavailable"))
          (cond ((eq op :set)
                 (shell-command* "source"
                   (concat venv "/bin/activate")
                   "&& pip config set global.index-url"
                   (nth (% n (length b)) b)))
                ((eq op :ls) (if (< n 0) b (nth (% n (length b)) b)))
                (t (let ((rc (shell-command* "source"
                               (concat venv "/bin/activate")
                               "&& pip config get global.index-url")))
                     (when (zerop (car rc))
                       (string-trim> (cdr rc)))))))))
    "Python pip mirror."))

(defmacro python*-lsp-spec ()
  "Return python lsp spec."
  `(let* ((venv (python*-venv-activate!))
          (pylsp (v-home% ".exec/pylsp.sh")))
     (unless venv
       (user-error "%s" "python venv unavailable"))
     (let ((rc (shell-command*
                   "chmod" "u+x"
                   (save-str-to-file
                    (concat
                     "#!/bin/sh\n"
                     "if pgrep -f $0 &>/dev/null; then\n"
                     "  exit 0\n"
                     "fi\n"
                     "source " venv "/bin/activate\n"
                     "if ! pip show python-lsp-server &>/dev/null; then\n"
                     "  pip install python-lsp-server\n"
                     "  exec $0 $@\n"
                     "fi\n"
                     "exec pylsp $@\n")
                    pylsp))))
       (if-feature-eglot%
           (when-var% eglot-command-history 'eglot
             (when (zerop (car rc))
               (push! pylsp eglot-command-history t)))
         (ignore* rc)))))

(unless-platform% 'windows-nt
  (defalias 'python*-lsp-make!
    (lexical-let*%
        ((b nil))
      (lambda (&optional op)
        (cond ((eq op :new) (setq b (python*-lsp-spec)))
              (t (inhibit-file-name-handler
                   (and b (file-exists-p b) b))))))
    "Make pylsp.sh for \\=`elgot\\='."))

;; end of lsp

(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; venv
  (unless-platform% 'windows-nt
    (when (python*-program)
      (python*-venv-activate!)
      (unless (python*-lsp-make!)
        (python*-lsp-make! :new))))
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
