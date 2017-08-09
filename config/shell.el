;;;; -*- lexical-binding:t -*-
;;;;
;; Shell environment base on OS
;;;;




(defmacro path-env-spec (spec)
  "Return the value of corresponding SPEC."
  (plist-get `(:source-file
               ,(concat (v-home* "config/") ".path-env.el")
               :compiled-file ,(concat (v-home* "config/") ".path-env.elc")
               :shell-name "bash"
               :shell-regexp ,(platform-supported-if windows-nt
                                  "/bash\.exe$"
                                "/bash$")
               :shell-path ,(platform-supported-if windows-nt
                                (bin-path "bash")
                              "/bin/bash")
               :path "PATH"
               :ld-path ,(platform-supported-unless windows-nt
                           (platform-supported-if darwin
                               "DYLD_LIBRARY_PATH"
                             (platform-supported-when gnu/linux
                               "LD_LIBRARY_PATH"))))
             spec))


(defmacro set-default-shell! (path regexp)
  "Set default SHELL"
  `(progn%
    (when (or (null shell-file-name)
              (not (string-match ,regexp shell-file-name)))
      (setq shell-file-name ,path))
    (when (or (null (getenv "SHELL"))
              (not (string-match ,regexp (getenv "SHELL"))))
      (setenv "SHELL" (file-name-base ,path)))))


(defmacro export-path-env! (var &optional add-to-exec-path)
  "Export path VAR or append to exec-path."
  `(when ,var
     (let ((env
            (platform-supported-if windows-nt
                (trim-right-newline (shell-command-to-string "echo $PATH"))
              (shell-command-to-string
               (concat "$SHELL -i -c 'echo -n $" ,var "' 2>/dev/null")))))
       (setenv ,var env)
       (when (and ,add-to-exec-path ,var)
         (let ((x (split-string env ":")))
           (while (car x)
             (add-to-list 'exec-path (car x) t #'string=)
             (setq x (cdr x))))))))


(defun save-path-env ()
  (export-path-env! (path-env-spec :path))
  (export-path-env! (path-env-spec :ld-path))
  (save-sexpr-to-file
   (list 'progn
         (list 'setenv (path-env-spec :path)
               (getenv (path-env-spec :path)))
         (list 'setq 'exec-path (list 'quote exec-path))
         (when (path-env-spec :ld-path)
           (list 'setenv (path-env-spec :ld-path) 
                 (getenv (path-env-spec :ld-path)))))
   (path-env-spec :source-file))
  (byte-compile-file (path-env-spec :source-file)))


(defmacro load-path-env ()
  `(progn
     (if (file-exists-p (path-env-spec :compiled-file))
         (load (path-env-spec :compiled-file))
       (export-path-env! (path-env-spec :path) t)
       (export-path-env! (path-env-spec :ld-path)))
     (add-hook 'kill-emacs-hook #'save-path-env)))


;; set shell on darwin
(platform-supported-when
    darwin
  (load-path-env))


;; set shell on Linux
(platform-supported-when
    gnu/linux
  (set-default-shell! (path-env-spec :shell-name)
                      (path-env-spec :shell-regexp))
  (load-path-env))


;; set shell on Windows
(platform-supported-when
    windows-nt

  
  (defmacro windows-nt-path (p)
    "Return the path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p))



  (when `(bin-exists-p ,(path-env-spec :shell-name))

    (defun set-windows-nt-shell! ()
      (setenv "SHELL" (path-env-spec :shell-path))
      (set-default-shell! (path-env-spec :shell-path)
                          (path-env-spec :shell-regexp))
      (load-path-env))

    (defadvice shell (before shell-before compile)
      (set-windows-nt-shell!))
    
    (defadvice ansi-term (around ansi-term-around compile)
      (let* ((n "*ansi-term*")
             (b (get-buffer-create n)))
        (apply 'make-comint-in-buffer n b "cmd" nil nil)
        (set-window-buffer (selected-window) b)))))


