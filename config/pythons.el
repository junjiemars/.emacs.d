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

;;; require

;; `shell-format-buffer'
(require 'ed (v-home%> "config/ed"))

;; end of require

;;; version/program

(defun python*-version (python)
  "Return the version of PYTHON"
  (let ((rc (shell-command* python "--version")))
    (when (zerop (car rc))
      (string-match* "^Python \\([.0-9]+\\)$" (cdr rc) 1))))

(defalias 'python*-program
  (let ((b (let ((p (or (executable-find% "python3")
                        (executable-find% "python"))))
             (when p (cons p (python*-version p))))))
    (lambda (&optional op n)
      (cond ((and op (eq :new op)) (setq b (cons n (python*-version n))))
            ((and op (eq :bin op)) (car b))
            ((and op (eq :ver op)) (cdr b))
            (t b))))
  "Adjoint of \\=`python-shell-interpreter\\='.")

;; end of version/program

;;;
;; venv
;;;

(defun python*-venv-activate! (dir)
  "Activate Python\\='s virtualenv at DIR.\n
PYTHONPATH: augment the default search path for module files. The
            format is the same as the shell’s PATH.
PYTHONHOME: change the location of the standard Python libraries.
VIRTUALENV: virtualenv root path.\n
After Python3.3+, we can use \\=`python -m venv DIR\\=' to create
a new virtual env at DIR, then Using \\=`sys.prefix\\=' to
determine whether inside a virtual env. Another way is using
\\=`pip -V\\='."
  (let ((pv (python*-program)))
    (unless pv
      (error "%s" "No python program found"))
    (let ((d (path! (path+ (expand-file-name dir))))
          (p (car pv))
          (v- (= -1 (vstrncmp (cdr pv) "3.3" 3))))
      (unless (file-exists-p (concat d "/bin/activate"))
        (cond ((and p v- (executable-find% "virtualenv"))
               (let ((rc (shell-command* "virtualenv" "-p" p d)))
                 (unless (zerop (car rc))
                   (error "Panic, %s" (string-trim> (cdr rc))))))
              ((and p (not v-))
               (let ((rc (shell-command* p "-m" "venv" d)))
                 (unless (zerop (car rc))
                   (error "Panic, %s" (string-trim> (cdr rc))))))
              (t (error "%s" "No python's venv found"))))
      (prog1 d
        (when-var% python-shell-interpreter python
          (setq python-shell-interpreter (concat d "bin/python")))
        (if-var% python-shell-virtualenv-root python
                 (setq python-shell-virtualenv-root d)
          (if-var% python-shell-virtualenv-path python
                   (setq python-shell-virtualenv-path d)
            (if-var% python-shell-process-environment python
                     (setq python-shell-process-environment
                           (list
                            (concat "PYTHONPATH=" d)
                            (concat "PYTHONHOME=" d)
                            (concat "VIRTUAL_ENV=" d)))
              (setenv "PYTHONPATH" d)
              (setenv "PYTHONHOME" d)
              (setenv "VIRTUAL_ENV" d))))))))

;; end of venv

;;;
;; lsp
;;;

(defconst +python*-pip-mirror+
  '("https://pypi.tuna.tsinghua.edu.cn/simple/"
    "https://pypi.mirrors.ustc.edu.cn/simple/"
    "http://pypi.hustunique.com/"
    "http://pypi.sdutlinux.org/")
  "List of pip mirror.")

(defun python*-pip-mirror! (venv &optional mirror)
  "Set pip MIRROR in VENV."
  (let* ((x (or (and (> (length mirror) 0) mirror)
                (catch 'br
                  (dolist (a +python*-pip-mirror+)
                    (let ((rc (shell-command* "curl" "-fsIL" a)))
                      (when (zerop (car rc))
                        (throw 'br a)))))
                (car +python*-pip-mirror+)))
         (rc (shell-command* "source"
               (concat venv "/bin/activate")
               "&& pip config set global.index-url"
               x)))
    (and (zerop (car rc)) x)))

(when-feature% eglot
  (defun python*-eglot-sever-program ()
    (let ((pylsp (python*-venv :pylsp)))
      (when (and pylsp (file-exists-p pylsp))
        (when-var% eglot-server-programs eglot
          (unless (boundp 'eglot-server-programs)
            (require 'eglot))
          (let ((ent (assoc '(python-mode python-ts-mode)
                            eglot-server-programs)))
            (when ent
              (setcdr ent (list pylsp))
              ent)))))))

(defun python*-pylsp-make! (venv pylsp)
  "Make PYLSP for VENV."
  (let ((rc (shell-command*
                "chmod" "u+x"
                (save-str-to-file
                 (concat
                  "#!/bin/sh\n"
                  ". " venv "bin/activate\n"
                  "if ! pip -qqq show python-lsp-server; then\n"
                  "  pip install python-lsp-server python-lsp-ruff pyflakes isort\n"
                  "fi\n"
                  "exec " venv "bin/pylsp $@\n")
                 pylsp))))
    (when (zerop (car rc))
      (prog1 pylsp
        (when-feature% eglot
          (python*-eglot-sever-program))))))

;; end of lsp

(defun python*-venv-scratch ()
  (let ((pyvenv (emacs-home% "scratch/pyvenv/")))
    (unless (file-exists-p pyvenv)
      (path! pyvenv))
    pyvenv))

(defalias 'python*-venv
  (let* ((b (python*-venv-scratch))
         (file (v-home% ".exec/python-venv.el"))
         (env (list :venv b
                    :pylsp (v-home% ".exec/pylsp.sh")
                    :python (concat b "bin/python")
                    :pip (concat b "bin/pip")
                    :mirror (python*-pip-mirror! b))))
    (lambda (&optional op n)
      (cond ((and op (eq op :file)) file)
            ((and (and op (eq op :load)) n) (setq env n))
            ((and op (eq op :scratch)) b)
            ((and (and op (eq op :venv)) n)
             (plist-put env :venv n)
             (plist-put env :python (concat n "bin/python"))
             (plist-put env :pip (concat n "bin/pip")))
            ((and op (eq op :venv)) (plist-get env :venv))
            ((and (and op (eq op :pylsp)) n) (plist-put env :pylsp n))
            ((and op (eq op :pylsp)) (plist-get env :pylsp))
            ((and op (eq op :python)) (plist-get env :python))
            ((and op (eq op :pip)) (plist-get env :pip))
            ((and (and op (eq op :mirror)) n) (plist-put env :mirror n))
            ((and op (eq op :mirror)) (plist-get env :mirror))
            (t env))))
  "Python\\='s venv.")

(defun python*-venv-make! (&optional dir mirror)
  "Make Python\\='s venv for DIR."
  (interactive (if current-prefix-arg
                   (list (read-directory-name "make venv for ")
                         (read-string "set pip mirror "))
                 (list (python*-venv :scratch)
                       (python*-venv :mirror))))
  (let* ((dir (or dir (python*-venv :scratch)))
         (mirror (or mirror (python*-venv :mirror)))
         (venv (python*-venv-activate! dir)))
    (unless venv
      (user-error "%s" "python venv unavailable"))
    (python*-venv :venv venv)
    (python*-venv :mirror (python*-pip-mirror! venv mirror))
    (python*-pylsp-make! venv (python*-venv :pylsp))
    (setq% python-shell-interpreter (python*-venv :python) python)
    (save-sexp-to-file (python*-venv) (python*-venv :file))))

(defun python*-format-buffer ()
  "Format the current buffer."
  (interactive)
  (shell-format-buffer `(python-mode python-ts-mode)
    (when-feature% eglot
      (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (catch 'br
          (call-interactively #'eglot-format-buffer)
          (throw 'br t))))
    (make-temp-file "py-fmt-src-" nil ".py")
    (lambda (src)
      (let ((x (shell-command* (concat (python*-venv :venv) "bin/ruff")
                 "format" src)))
        (and (zerop (car x)) src)))))



;;;
;; keys
;;;

(defun python*-define-keys (keymap)
  (define-key keymap (kbd% "C-c C-z") #'run-python)
  (define-key keymap (kbd% "C-c M-c f") #'python*-format-buffer)
  (define-key keymap (kbd% "C-c C-j") nil))

;; end of keys

(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; interpreter
  (when (python*-program)
    (let ((interpreter
           (or (let ((f (python*-venv :file)))
                 (when (file-exists-p f)
                   (python*-venv :load (read-sexp-from-file f))
                   (when (file-exists-p (python*-venv :python))
                     (prog1 (python*-venv :python)
                       (python*-eglot-sever-program)))))
               (python*-program :bin))))
      (setq% python-shell-interpreter interpreter python)
      (setq% python-interpreter interpreter python)))
  ;; completion
  (setq% python-shell-completion-native-enable
         (when-platform% gnu/linux t) python)
  ;; keys
  (when-var% python-mode-map python
    (python*-define-keys python-mode-map))
  (when-var% python-ts-mode-map python-ts-mode
    (python*-define-keys python-ts-mode-map)))




(provide 'pythons)

;; end of pythons.el
