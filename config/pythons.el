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
  (lexical-let% ((b (let ((p (or (executable-find% "python3")
                                 (executable-find% "python"))))
                      (when p (cons p (python*-version p))))))
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
          (v- (= -1 (version-string= (cdr pv) "3.3"))))
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
              (setenv "VIRTUAL_ENV" d))))))))

;; end of venv

;;;
;; lsp
;;;

(defalias 'python*-pip-mirror
  (lexical-let% ((b '("https://pypi.tuna.tsinghua.edu.cn/simple/"
                      "https://pypi.mirrors.ustc.edu.cn/simple/"
                      "http://pypi.hustunique.com/"
                      "http://pypi.sdutlinux.org/")))
    (lambda (&optional n)
      (cond (n (nth (% n (length b)) b))
            (t b))))
  "List pip mirror.")

(defun python*-pip-mirror! (venv &optional mirror)
  "Set pip MIRROR in VENV."
  (let* ((x (or (and (> (length mirror) 0) mirror)
                (catch 'br
                  (dolist* (a (python*-pip-mirror))
                    (let ((rc (shell-command* "curl" "-fsIL" a)))
                      (when (zerop (car rc))
                        (throw 'br a)))))
                (python*-pip-mirror 0)))
         (rc (shell-command* "source"
               (concat venv "/bin/activate")
               "&& pip config set global.index-url"
               x)))
    (and (zerop (car rc)) x)))

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
        (when-feature-eglot%
          (when-var% eglot-command-history 'eglot
            (push! pylsp eglot-command-history t)))))))

;; end of lsp

(defalias 'python*-venv
  (lexical-let*%
      ((b (path! `,(emacs-home* "scratch/pyvenv/")))
       (file (v-home% ".exec/python-venv.el"))
       (env (list :venv b
                  :pylsp (v-home% ".exec/pylsp.sh")
                  :python (concat b "bin/python")
                  :pip (concat b "bin/pip")
                  :mirror (python*-pip-mirror! b))))
    (lambda (&optional op n)
      (cond ((eq op :file) file)
            ((and (eq op :load) n) (setq env n))
            ((eq op :scratch) b)
            ((and (eq op :venv) n)
             (plist-put env :venv n)
             (plist-put env :python (concat n "bin/python"))
             (plist-put env :pip (concat n "bin/pip")))
            ((eq op :venv) (plist-get env :venv))
            ((and (eq op :pylsp) n) (plist-put env :pylsp n))
            ((eq op :pylsp) (plist-get env :pylsp))
            ((eq op :python) (plist-get env :python))
            ((eq op :pip) (plist-get env :pip))
            ((and (eq op :mirror) n) (plist-put env :mirror n))
            ((eq op :mirror) (plist-get env :mirror))
            (t env))))
  "Python\\='s venv.")

(defun python*-venv-make! (&optional dir mirror)
  "Make Python\\='s venv for DIR."
  (interactive (if current-prefix-arg
                   (list (read-directory-name "make venv for ")
                         (read-string "set pip mirror "))
                 (list (python*-venv :scratch)
                       (python*-venv :mirror))))
  (let ((venv (python*-venv-activate! dir)))
    (unless venv
      (user-error "%s" "python venv unavailable"))
    (python*-venv :venv venv)
    (python*-venv :mirror (python*-pip-mirror! venv mirror))
    (python*-pylsp-make! venv (python*-venv :pylsp))
    (setq% python-shell-interpreter (python*-venv :python) 'python)
    (save-sexp-to-file (python*-venv) (python*-venv :file))))


(defun python*-format-buffer ()
  "Format the current buffer."
  (interactive)
  (with-current-buffer (current-buffer)
    (when (eq major-mode 'python-mode)
      (when-feature-eglot%
        (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
          (catch 'br
            (call-interactively #'eglot-format-buffer)
            (throw 'br t))))
      (let* ((p (point))
             (bounds (if-region-active
                         (cons (region-beginning) (region-end))
                       (cons (point-min) (point-max))))
             (rs (buffer-substring (car bounds) (cdr bounds)))
             (f (save-str-to-file
                 rs (concat (make-temp-name ".py-fmt-") ".py")))
             (ss (let ((x (shell-command*
                              (concat (python*-venv :venv) "bin/ruff")
                            "format"
                            f)))
                   (and (= 0 (car x))
                        (read-str-from-file f)))))
        (unless (string= rs ss)
          (save-excursion
            (delete-region (car bounds) (cdr bounds))
            (insert ss))
          (goto-char p))
        (when (file-exists-p f) (delete-file f))))))



(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; interpreter
  (when (python*-program)
    (let ((interpreter
           (or (let ((f (python*-venv :file)))
                 (and (file-exists-p f)
                      (python*-venv :load (read-sexp-from-file f))
                      (python*-venv :python)))
               (python*-program :bin))))
      (setq% python-shell-interpreter interpreter 'python)
      (setq% python-interpreter interpreter 'python)))

  ;; completion
  (setq% python-shell-completion-native-enable
         (when-platform% 'gnu/linux t) 'python)
  ;; keys
  (when-var% python-mode-map 'python
    (define-key python-mode-map (kbd "C-c C-z") #'run-python)
    (define-key python-mode-map (kbd "C-c M-c f") #'python*-format-buffer)))



(provide 'pythons)

;; end of pythons.el
