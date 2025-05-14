;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; pythons.el
;;;;
;; features:
;;; 1. probe Python environment.
;;; 2. make Python's virtual environment.
;;; 3. support `LSP'.
;;; 4. format code.
;;;;
;; Python help instructions:
;;; help:    help(help)
;;; dir:     list of strings, dir(sys.prefix)
;;; inspect: inspect living objects, needs import
;;; dis:     disassembler
;;;;
;; use cases:
;;; 1. ordinary.
;;; 2. venv: M-x `python*-venv-make!'.
;;; 3. switch Python's version: M-ESC `(python*-program <path>)'.
;;;;


;;; require

 ;; end of require

;;; env

(defun python*-version (python)
  "Return the version of PYTHON"
  (let ((rc (shell-command* python "--version")))
    (when (= 0 (car rc))
      (string-match* "^Python \\([.0-9]+\\)$" (cdr rc) 1))))

(defun python*-program-check ()
  (let ((p (or (executable-find* "python3")
               (executable-find* "python"))))
    (cond (p (vector p (python*-version p)))
          (t [nil nil]))))

(defalias 'python*-program
  (let ((b (python*-program-check)))
    (lambda (&optional op n)
      (cond ((and op (eq :bin op)) (aref b 0))
            ((and op (eq :ver op)) (aref b 1))
            ((and n op (eq :new op))
             (aset b 0 n) (aset b 1 (python*-version n)))
            (t b))))
  "Adjoint of \\=`python-shell-interpreter\\='.")

(defconst +python*-pip-mirror+
  '("https://pypi.tuna.tsinghua.edu.cn/simple/"
    "https://pypi.mirrors.ustc.edu.cn/simple/"
    "http://pypi.hustunique.com/"
    "http://pypi.sdutlinux.org/")
  "List of pip mirror.")

(defvar *python-mirror-history* +python*-pip-mirror+
  "Python mirror history list.")

 ;; end of env

;;;
;; LSP
;;;

(defun python*-pip-mirror! (venv &optional mirror)
  "Set pip MIRROR in VENV."
  (let* ((x (or (and (> (length mirror) 0) mirror)
                (catch :br
                  (dolist (a +python*-pip-mirror+)
                    (let ((rc (shell-command* "curl" "-fsIL" a)))
                      (when (= 0 (car rc))
                        (throw :br a)))))
                (car +python*-pip-mirror+)))
         (rc (shell-command* "source"
               (path+ venv "/bin/activate")
               "&& pip config set global.index-url"
               x)))
    (unless (and (= 0 (car rc)) x)
      (user-error "Panic, %s" (string-trim> (cdr rc))))
    x))

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
  (set-file-modes
   (write-str-to-file
    (format (read-str-from-file (emacs-home* "config/pythons_lsp.sh"))
            venv venv)
    pylsp)
   #o744)
  (when-feature% eglot
    (python*-eglot-sever-program))
  pylsp)

;; end of LSP

;;;
;; venv
;;;

(defun python*--interpreter! (interpreter)
  (setq% python-shell-interpreter interpreter python)
  (setq% python-interpreter interpreter python))

(defun python*--env-build (dir &optional mirror)
  (list :venv dir
        :pylsp (v-home% ".exec/pylsp.sh")
        :python (path+ dir "/bin/python")
        :pip (path+ dir "/bin/pip")
        :fmt (path+ dir "/bin/ruff")
        :mirror mirror))

(defun python*--venv-prompt ()
  (if current-prefix-arg
      (list (read-file-name "Python executable at ")
            (read-directory-name "Make venv for ")
            (read-string "Has a pip mirror "
                         (car *python-mirror-history*)
                         '*python-mirror-history*))
    (list (python*-program :bin)
          (python*-venv :scratch)
          nil)))

(defun python*--venv-activate! (dir)
  "Activate Python\\='s virtualenv at DIR.\n
Using \\=`sys.prefix\\=' or \\=`pip -V\\=' to check virtual env."
  (let ((p (python*-program :bin))
        (v (python*-program :ver)))
    (unless (or p v)
      (error "%s" "No python program found"))
    (let ((d (path! (path+ (expand-file-name dir) "/")))
          (v- (= -1 (vstrncmp v "3.3" 3))))
      (unless (file-exists-p (path+ d "/bin/activate"))
        (cond ((and p v- (executable-find* "virtualenv"))
               (let ((rc (shell-command* "virtualenv" "-p" p d)))
                 (unless (= 0 (car rc))
                   (error "Panic, %s" (string-trim> (cdr rc))))))
              ((and p (null v-))
               (let ((rc (shell-command* p "-m" "venv" d)))
                 (unless (= 0 (car rc))
                   (error "Panic, %s" (string-trim> (cdr rc))))))
              (t (error "%s" "No python's venv found"))))
      (prog1 d
        (python*--interpreter! (path+ d "/bin/python"))
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

(defalias 'python*-venv
  (let* ((b (path! (emacs-home% "scratch/pyvenv/")))
         (file (v-home% ".exec/python-env.el"))
         (env (python*--env-build b)))
    (lambda (&optional op n)
      (cond ((and op (eq op :file)) file)
            ((and op (eq op :scratch)) b)
            ((and op (eq op :venv)) (plist-get env :venv))
            ((and op (eq op :pylsp)) (plist-get env :pylsp))
            ((and op (eq op :python)) (plist-get env :python))
            ((and op (eq op :pip)) (plist-get env :pip))
            ((and op (eq op :mirror)) (plist-get env :mirror))
            ((and op (eq op :fmt)) (plist-get env :fmt))
            ((and n op (eq op :load)) (setq env n))
            (t env))))
  "Python\\='s venv.")

(defun python*-venv-make! (&optional python dir mirror)
  "Make Python\\='s venv for DIR."
  (interactive (python*--venv-prompt))
  (python*-program :new python)
  (unless (python*-program :ver)
    (user-error "%s is not an executable" python))
  (let* ((venv (python*--venv-activate! dir))
         (mirror (python*-pip-mirror! venv mirror)))
    (python*-venv :load (python*--env-build venv mirror))
    (python*-pylsp-make! venv (python*-venv :pylsp))
    (write-sexp-to-file (python*-venv) (python*-venv :file))))

;; end of venv

;;;
;; format
;;;

(defun python*-format-region (&optional beg end)
  "Format the region of current buffer via ruff."
  (interactive (select-region-prompt))
  (let ((cur (point)))
    (unwind-protect
        (let ((src (buffer-substring-no-properties beg end))
              (tmp (make-temp-file "py-fmt-")))
          (write-str-to-file src tmp)
          (let ((x (shell-command* (python*-venv :fmt) "format" tmp)))
            (when (= 0 (car x))
              (with-current-buffer (current-buffer)
                (delete-region beg end)
                (insert (read-str-from-file tmp))))))
      (goto-char cur)
      cur)))


;; end of format

;;;
;; init
;;;

(defun python*-define-keys (keymap)
  (define-key keymap (kbd% "C-c C-z") #'run-python)
  (define-key keymap (kbd% "C-c C-j") nil)
  (define-key keymap (kbd% "C-c M-c f") #'python*-format-region))

(defun on-python-init! ()
  "On \\=`python\\=' initialization."
  ;; interpreter
  (when (python*-program)
    (let ((interpreter
           (or (let ((f (python*-venv :file)))
                 (when (file-exists-p f)
                   (python*-venv :load (read-sexp-from-file f))
                   (when (condition-case _
                             (python*--venv-activate! (python*-venv :venv))
                           (error nil))
                     (prog1 (python*-venv :python)
                       (when-feature% eglot
                         (python*-eglot-sever-program))))))
               (python*-program :bin))))
      (python*--interpreter! interpreter)))
  ;; completion
  (setq% python-shell-completion-native-enable
         (when-platform% gnu/linux t) python)
  ;; keys
  (when-var% python-mode-map python
    (python*-define-keys python-mode-map))
  (when-var% python-ts-mode-map python-ts-mode
    (python*-define-keys python-ts-mode-map))
  t)

;; end of init


(provide 'pythons)

;; end of pythons.el
