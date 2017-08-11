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
               :shell-path ,(bin-path "bash")
               :shell-var "SHELL"
               :path-var "PATH"
               :ld-path-var ,(platform-supported-unless windows-nt
                               (platform-supported-if darwin
                                   "DYLD_LIBRARY_PATH"
                                 (platform-supported-when gnu/linux
                                   "LD_LIBRARY_PATH")))
               :echo-format
               ,(platform-supported-if windows-nt
                    "echo %%%s%% 2>/nul"
                  "$SHELL -i -l -c 'echo -n $%s' 2>/dev/null"))
             spec))


(defvar *default-path-env* nil
  "Default path environments, get via (path-env-> k) and put via (path-env<- k v) ")


(defmacro path-env-> (k)
  "Extract the value from `*default-path-env*' via K."
  `(plist-get *default-path-env* ,k))

(defmacro path-env<- (k v)
  "Change the value in `*default-path-env* via K."
  `(plist-put *default-path-env* ,k ,v))


(defmacro echo-var (var &optional transfer)
  "Echo a $VAR, and then TRANSFER it if there has one."
  `(let ((v (shell-command-to-string
             (format (path-env-spec :echo-format) ,var))))
     (if (and ,transfer (functionp ,transfer))
         (funcall ,transfer v)
       v)))


(defmacro refine-path (path)
  "Refine PATH, non empty or non exists."
  `(when (consp ,path)
     (delete nil
             (mapcar (lambda (x)
                       (when (and (not (null x))
                                  (not (string= "" x))
                                  (file-exists-p x))
                         x)) ,path))))


(defmacro path->var (path sep)
  "Convert a list of PATH to $PATH var, base on SEP."
  `(let ((p nil))
     (dolist (x ,path)
       (setq p (concat p (when p ,sep) x)))
     p))


(defun save-path-env! ()
  (setq *default-path-env*
        (list :path
              (refine-path
               (split-string
                (echo-var (path-env-spec :path-var)
                          (lambda (x)
                            (replace-regexp-in-string
                             "[ ]*\n$" "" x)))
                path-separator))
              :ld-path (platform-supported-unless windows-nt
                         (refine-path
                          (split-string
                           (echo-var (path-env-spec :ld-path-var)
                                     (lambda (x)
                                       (replace-regexp-in-string
                                        "[ ]*\n$" "" x)))
                           path-separator)))
              :shell-file-name nil))
  (save-sexpr-to-file
   (list 'setq '*default-path-env*
         (list 'list
               ':path (list 'quote (path-env-> :path))
               ':ld-path (platform-supported-unless windows-nt
                           (list 'quote (path-env-> :ld-path)))
               ':shell-file-name nil))
   (path-env-spec :source-file))
  (byte-compile-file (path-env-spec :source-file)))


(defmacro load-path-env! ()
  `(progn
     (when (file-exists-p (path-env-spec :compiled-file))
       (load (path-env-spec :compiled-file)))
     (add-hook 'kill-emacs-hook #'save-path-env!)))


;; set shell on darwin
(platform-supported-when
    darwin
  (load-path-env!)
  (setenv (path-env-spec :path-var)
          (path->var (path-env-> :path) path-separator)))


;; set shell on Linux
(platform-supported-when
    gnu/linux
  (load-path-env!)
  (setenv (path-env-spec :shell-var)
          (path-env-spec :shell-path)))


;; set shell on Windows
(platform-supported-when
    windows-nt

  (defmacro windows-nt-path (p)
    "Return the path that windows-nt can recoganized."
    `(replace-regexp-in-string "\\\\" "/" ,p))

  
  (defadvice ansi-term (around ansi-term-around compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (setenv (path-env-spec :shell-var) (path-env-> :shell-file-name))
      (setq shell-file-name (getenv (path-env-spec :shell-var)))
      (setenv (path-env-spec :path-var)
              (path->var (path-env-> :path) path-separator))
      (setq shell-file-name (path-env-> :shell-file-name))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b)))

  
  (when `(bin-exists-p ,(path-env-spec :shell-name))

    (load-path-env!)
    (path-env<- :shell-file-name shell-file-name)
    
    (defmacro windows-nt-posix-path (p)
      "Retrun the posix path that shell can regcoganized on windows-nt."
      `(replace-regexp-in-string "\\([a-zA-Z]\\):/" "/\\1/"
                                 (windows-nt-path ,p)))

    (defadvice shell (before shell-before compile)
      (setenv (path-env-spec :shell-var) (path-env-spec :shell-path))
      (setenv (path-env-spec :path-var)
              (windows-nt-posix-path (path->var (path-env-> :path) ":")))
      (setq shell-file-name (getenv (path-env-spec :shell-var))))))
