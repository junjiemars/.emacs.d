;;;; -*- lexical-binding:t -*-
;;;;
;; Shell environment base on OS
;;;;




(defmacro path-env-spec ()
  "Return a `plist' of virtualized env-spec."
  `(let ((*v* ,(v-home* "config/")))
     (list :source-file (concat *v* ".path-env.el")
           :compiled-file (concat *v* ".path-env.elc")
           :path-var "PATH"
           :lib-path-var
           (platform-supported-unless windows-nt
             (platform-supported-if darwin
                 "DYLD_LIBRARY_PATH"
               (platform-supported-when gnu/linux
                 "LD_LIBRARY_PATH"))))))


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


(defun save-path-env ()
  (export-path-env! (plist-get (path-env-spec) :path-var))
  (export-path-env! (plist-get (path-env-spec) :lib-path-var))
  (save-sexpr-to-file
   (list 'progn
         (list 'setenv (plist-get (path-env-spec) :path-var)
               (getenv (plist-get (path-env-spec) :path-var)))
         (list 'setq 'exec-path (list 'quote exec-path))
         (list 'setenv (plist-get (path-env-spec) :lib-path-var)
               (getenv (plist-get (path-env-spec) :lib-path-var))))
   (plist-get (path-env-spec) :source-file))
  (byte-compile-file (plist-get (path-env-spec) :source-file)))


(defmacro load-path-env ()
  (if (file-exists-p (plist-get (path-env-spec) :compiled-file))
      (load (plist-get (path-env-spec) :compiled-file))
    (export-path-env! (plist-get (path-env-spec) :path-var) t)
    (export-path-env! (plist-get (path-env-spec) :lib-path-var)))
  (add-hook 'kill-emacs-hook #'save-path-env))


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


