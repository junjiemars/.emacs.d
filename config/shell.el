;;;;
;; Shell environment base on OS
;;;;




(defmacro save-sexpr-to-file (sexpr filename)
  "Save `sexpr' to a file"
  `(save-excursion
     (let ((sexpr-buffer (find-file-noselect ,filename)))
       (set-buffer sexpr-buffer)
       (erase-buffer)
       (print ,sexpr sexpr-buffer)
       (save-buffer)
       (kill-buffer sexpr-buffer))))


;; Non-Windows OS shell env
(platform-supported-unless
    windows-nt

  
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
       (while (car x)
         (add-to-list 'exec-path (car x) t #'string=)
         (setq x (cdr x)))))


  (defmacro path-env ()
    "Return the `cons' of virtualized `path-env' source and compiled file name."
    (let ((_v_ (make-vdir "config/")))
      `(cons ,(concat _v_ ".path-env.el")
             ,(concat _v_ ".path-env.elc"))))

  
  (defmacro save-path-env ()
    `(add-hook 'kill-emacs-hook
               (lambda ()
                 (let ((env (path-env)))
                   (dolist (f env)
                     (when (file-exists-p f)
                       (delete-file f)))
                   (set-path-env)
                   (save-sexpr-to-file
                    (list 'progn
                          (list 'setenv "PATH" (getenv "PATH"))
                          (list 'setq 'exec-path (list 'quote exec-path)))
                    (car env))
                   (byte-compile-file (car env))))))


  (defmacro load-path-env ()
    `(let ((env (path-env)))
       (if (file-exists-p (cdr env))
           (load (cdr env))
         (set-path-env))
       (save-path-env))))


;; set PATH on darwin
(platform-supported-when
    darwin
  (load-path-env))


;; set PATH on Linux
(platform-supported-when
 gnu/linux
 (set-default-shell "/bin/bash" "\/bash$")
 (load-path-env))


;; set shell/ansi-term on Windows
(platform-supported-when
    windows-nt

  (defmacro windows-nt-path (p)
    "Return the path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p))

  
  (defun set-windows-nt-shell ()
    (when (bin-exists-p "bash")
      (let ((prompt "~/.emacs_bash"))
        (unless (file-exists-p prompt)
          (copy-file "~/.emacs.d/config/.emacs_bash" prompt)))
      (add-to-list 'exec-path
                   (file-name-directory
                    (windows-nt-path (bin-path "bash"))))
      (setq shell-file-name "bash")
      (setenv "SHELL" (bin-path "bash"))))
  
  (defadvice shell (before shell-before compile)
    (set-windows-nt-shell))
  
  (defadvice ansi-term (around ansi-term-around compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b))))


