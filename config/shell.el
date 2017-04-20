;;;;
;; Shell
;;;;


;; Setup shell environment base on OS


(defmacro set-default-shell (shell rshell)
  "Set default SHELL"
  `(progn
     (when (or (null (getenv "SHELL"))
               (not (string-match ,rshell (getenv "SHELL"))))
       (setenv "SHELL" ,shell))
     (when (or (null shell-file-name)
               (not (string-match ,rshell shell-file-name)))
       (setq shell-file-name ,shell))))


(defmacro set-path-env ()
  "Set PATH and exec-path in Emacs."
  `(let* ((p (shell-command-to-string
              "$SHELL -i -c 'echo -n $PATH' 2>/dev/null"))
          (x (split-string p ":")))
     (setenv "PATH" p)
     (setq path-env-var p)
     (while (car x)
       (add-to-list 'exec-path (car x) t #'string=)
       (setq x (cdr x)))))


(defvar compiled-path-env-file (concat (make-vdir "config/") ".path-env.elc"))
(defvar path-env-file (concat (make-vdir "config/") ".path-env.el"))
(defvar path-env-var nil)

(defmacro save-path-env ()
  `(add-hook 'kill-emacs-hook
             (lambda ()
               (when (file-exists-p path-env-file)
                 (delete-file path-env-file))
               (when (file-exists-p compiled-path-env-file)
                 (delete-file compiled-path-env-file))
               (set-path-env)
               (save-expr-to-file
                (list 'progn
                      (list 'setenv "PATH" path-env-var)
                      (list 'setq 'exec-path (list 'quote exec-path)))
                path-env-file)
               (byte-compile-file path-env-file))))


(defmacro load-path-env ()
  `(progn
     (if (file-exists-p path-env-file)
         (load path-env-file)
       (set-path-env))
     (save-path-env)))


;; set PATH on darwin
(platform-supported-p
    darwin
  (load-path-env))


;; set PATH on Linux
(platform-supported-p
    gnu/linux
  (set-default-shell "/bin/bash" "\/bash$")
  (load-path-env))


;; set shell/ansi-term on Windows
(platform-supported-p
    windows-nt
  (defadvice shell (before shell-before compile)
    (when (bin-exists-p "bash")
      (let ((prompt "~/.emacs_bash"))
        (unless (file-exists-p prompt)
          (copy-file "~/.emacs.d/config/.emacs_bash" prompt)))
      (add-to-list 'exec-path
                   (file-name-directory
                    (windows-nt-path (bin-path "bash"))))
      (setq shell-file-name "bash")
      (setenv "SHELL" (bin-path "bash"))))
  (defadvice ansi-term (around ansi-term-around compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b))))


