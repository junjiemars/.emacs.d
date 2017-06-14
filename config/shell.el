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

  
(defmacro set-path-env! ()
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
  (let ((*v* (v-home* "config/")))
    `(cons ,(concat *v* ".path-env.el")
           ,(concat *v* ".path-env.elc"))))

  
(defun save-path-env ()
  (let ((env (path-env)))
    (set-path-env!)
    (save-sexpr-to-file
     (list 'progn
           (list 'setenv "PATH" (getenv "PATH"))
           (list 'setq 'exec-path (list 'quote exec-path)))
     (car env))
    (byte-compile-file (car env))))


(defmacro load-path-env ()
  `(let ((env (path-env)))
     (if (file-exists-p (cdr env))
         (load (cdr env))
       (set-path-env!))
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


