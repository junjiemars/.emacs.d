;;;; -*- lexical-binding:t -*-
;;;;
;; Shell environment base on OS
;;;;




(defmacro set-default-shell! (path regexp)
  "Set default SHELL"
  `(progn%
    ,(when (or (null (getenv "SHELL"))
               (not (string-match regexp (getenv "SHELL"))))
       `(setenv "SHELL" (file-name-base ,path)))
    ,(when (or (null shell-file-name)
               (not (string-match regexp shell-file-name)))
       `(setq shell-file-name ,path))))


(defmacro export-path-env! (var &optional add-to-exec-path)
  "Export path VAR or append to exec-path."
  `(let ((env (shell-command-to-string
               (concat "$SHELL -i -c 'echo -n $" ,var "' 2>/dev/null"))))
     (setenv ,var env)
     (when (and ,add-to-exec-path ,var)
       (let ((x (split-string env path-separator)))
         (while (car x)
           (add-to-list 'exec-path (car x) t #'string=)
           (setq x (cdr x)))))))


(defmacro path-env ()
  "Return the `cons' of virtualized `path-env' source and compiled file name."
  (let ((*v* (v-home* "config/")))
    `(cons ,(concat *v* ".path-env.el")
           ,(concat *v* ".path-env.elc"))))

  
(defun save-path-env ()
  (let ((env (path-env)))
    (export-path-env! "PATH")
    (export-path-env! "LD_LIBRARY_PATH")
    (save-sexpr-to-file
     (list 'progn
           (list 'setenv "PATH" (getenv "PATH"))
           (list 'setq 'exec-path (list 'quote exec-path))
           (list 'setenv "LD_LIBRARY_PATH" (getenv "LD_LIBRARY_PATH")))
     (car env))
    (byte-compile-file (car env))))


(defmacro load-path-env ()
  `(let ((env (path-env)))
     (if (file-exists-p (cdr env))
         (load (cdr env))
       (export-path-env! "PATH" t)
       (export-path-env! "LD_LIBRARY_PATH"))
     (add-hook 'kill-emacs-hook #'save-path-env)))


;; set shell on darwin
(platform-supported-when
    darwin
  (load-path-env))


;; set shell on Linux
(platform-supported-when
    gnu/linux
  (set-default-shell! "/bin/bash" "\/bash$")
  (load-path-env))


;; set shell on Windows
(platform-supported-when
    windows-nt

  
  (defmacro windows-nt-path (p)
    "Return the path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p))


  (when (bin-exists-p "bash")
    
    (defun set-windows-nt-shell! ()
      (setenv "SHELL" (bin-path "bash"))
      (set-default-shell! (windows-nt-path (bin-path "bash")) "/bash\.exe$")
      (load-path-env))


    (defadvice shell (before shell-before compile)
      (set-windows-nt-shell!))

    
    (defadvice ansi-term (around ansi-term-around compile)
      (let* ((n "*ansi-term*")
             (b (get-buffer-create n)))
        (apply 'make-comint-in-buffer n b "cmd" nil nil)
        (set-window-buffer (selected-window) b)))))


