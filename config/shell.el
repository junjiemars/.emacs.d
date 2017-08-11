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
               :shell-path ,(bin-path "bash")
               :path "PATH"
               :ld-path ,(platform-supported-unless windows-nt
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
  "Default path environments")


(defmacro path-env-> (k)
  `(plist-get *default-path-env* ,k))

(defmacro path-env<- (k v)
  `(plist-put *default-path-env* ,k ,v))


(defmacro echo-var (var &optional transfer)
  `(let ((v (shell-command-to-string
             (format (path-env-spec :echo-format) ,var))))
     (if (and ,transfer (functionp ,transfer))
         (funcall ,transfer v)
       v)))


(defmacro refine-path (path)
  `(when (consp ,path)
     (delete nil
             (mapcar (lambda (x)
                       (when (and (not (null x))
                                  (not (string= "" x))
                                  (file-exists-p x))
                         x)) ,path))))


(defmacro path->var (path sep)
  `(let ((p nil))
     (dolist (x ,path)
       (setq p (concat p (when p ,sep) x)))
     p))


(defun save-path-env! ()
  (unless *default-path-env*
    (setq *default-path-env*
          (list :path
                (refine-path
                 (split-string
                  (echo-var (path-env-spec :path)
                            (lambda (x)
                              (replace-regexp-in-string
                               "[ ]*\n$" "" x)))
                  path-separator))
                :ld-path (platform-supported-unless windows-nt
                           (refine-path
                            (split-string
                             (echo-var (path-env-spec :ld-path)
                                       (lambda (x)
                                         (replace-regexp-in-string
                                          "[ ]*\n$" "" x)))
                             path-separator)))
                :exec-path nil
                :shell-file-name (platform-supported-when windows-nt
                                   (unless (path-env-> :shell-file-name)
                                     shell-file-name))))
    (path-env<- :exec-path (append
                            (path-env-> :path)
                            (refine-path exec-path))))
  (save-sexpr-to-file
   (list 'setq '*default-path-env*
         (list 'list
               ':path (list 'quote (path-env-> :path))
               ':ld-path (list 'quote (path-env-> :ld-path))
               ':exec-path (list 'quote (path-env-> :exec-path))
               ':shell-file-name (path-env-> :shell-file-name)))
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
  (setenv (path-env-spec :path) (path->var (path-env-> :path) path-separator)))


;; set shell on Linux
(platform-supported-when
    gnu/linux
  (load-path-env!)
  (setenv "SHELL" (path-env-spec :shell-path)))


;; set shell on Windows
(platform-supported-when
    windows-nt

  (when `(bin-exists-p ,(path-env-spec :shell-name))

    (load-path-env!)
    
    (defmacro windows-nt-path (p)
      "Return the path that windows-nt can recoganized."
      `(replace-regexp-in-string "\\\\" "/" ,p))
    
    
    (defadvice shell (before shell-before compile)
      (when (consp path)
        (setenv "SHELL" (path-env-spec :shell-path))
        (set-default-shell! (path-env-spec :shell-path)
                            (path-env-spec :shell-regexp))
        (path->var (path-env-> :path) ":")))
    
    (defadvice ansi-term (around ansi-term-around compile)
      (let* ((n "*ansi-term*")
             (b (get-buffer-create n)))
        (setenv "SHELL" (path-env-> :shell-file-name))
        (path->var (path-env-> :path) path-separator)
        (apply 'make-comint-in-buffer n b "cmd" nil nil)
        (set-window-buffer (selected-window) b)))))
