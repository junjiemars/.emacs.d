;;;; -*- lexical-binding:t -*-
;;;;
;; Shell environment base on OS
;;;;


(defmacro shell-env-spec->% (&rest keys)
  "Extract a value from the list of virtualized `path-env-spec' 
via KEYS at compile time."
  (let ((spec `(list
                :source-file
                ,(concat (v-home* "config/") ".shell-env.el")
                
                :compiled-file
                ,(concat (v-home* "config/") ".shell-env.elc")
                
                :shell-path ,(bin-path "bash")
                :shell-var "SHELL"
                :path-var "PATH"
                
                :echo-format
                ,(platform-supported-if windows-nt
                     "echo %%%s%% 2>/nul"
                   `'((:interactive-shell
                       . "$SHELL -l -i -c 'echo -n $%s' 2>/dev/null")
                      (:login-shell
                       . "$SHELL -l -c 'echo -n $%s' 2>/dev/null"))))))
    `(self-spec->% ,spec ,@keys)))


(defvar *default-shell-env*
  (list :path nil
        :shell-file-name nil
        :exec-path nil
        :env-vars nil)
  "default path environments, 
get via (path-env-> k) and put via (path-env<- k v) ")


(defmacro shell-env-> (&optional k)
  "extract the value from `*default-shell-env*' via k."
  `(if ,k
       (plist-get *default-shell-env* ,k)
     *default-shell-env*))

(defmacro path-env<- (k v)
  "change the value of `*default-shell-env* via k and v."
  `(plist-put *default-shell-env* ,k ,v))

(defmacro self-spec->*shell (&rest keys)
  `(self-safe-call*
    "env-spec"
    (self-spec->* :shell ,@keys)))

(defmacro echo-var (var &optional echo-format)
  "echo a $var."
  `(shell-command-to-string
    (format (if ,echo-format ,echo-format
              (platform-supported-if windows-nt
                  (shell-env-spec->% :echo-format)
                (if (self-spec->*shell :interactive-shell)
                    (alist-get-> :interactive-shell
                                 (shell-env-spec->% :echo-format))
                  (alist-get-> :login-shell
                               (shell-env-spec->% :echo-format)))))
            ,var)))


(defmacro paths->var (path sep)
  "convert a list of path to $path like var that separated by sep."
  `(let (p)
     (dolist (x ,path p)
       (setq p (concat p (when p ,sep) x)))))

(defmacro var->paths (var)
  "refine var like $path to list by `path-separator'."
  `(split-string>< ,var path-separator t "[ ]+\n"))





(defun save-shell-env! ()
  (path-env<- :path (echo-var (shell-env-spec->% :path-var)))
  (path-env<- :shell-file-name nil)
  (path-env<- :exec-path (dolist
                             (p (var->paths (shell-env-> :path)) exec-path)
                           (add-to-list 'exec-path p t #'string=)))
  (path-env<- :env-vars (let ((vars (self-spec->*shell :env-vars))
                              (x nil))
                          (dolist (v vars x)
                            (push (cons v (echo-var v)) x))))
  (save-sexp-to-file
   (list 'setq '*default-shell-env*
         (list 'list
               ':path (shell-env-> :path)
               ':shell-file-name nil
               ':exec-path (list 'quote (shell-env-> :exec-path))
               ':env-vars (list 'quote (shell-env-> :env-vars))))
   (shell-env-spec->% :source-file))
  (byte-compile-file (shell-env-spec->% :source-file)))


(defmacro load-shell-env! ()
  `(progn
     (if (file-exists-p (shell-env-spec->% :compiled-file))
         (load (shell-env-spec->% :compiled-file))
       (path-env<- :path (getenv (shell-env-spec->% :path-var))))
     (add-hook 'kill-emacs-hook #'save-shell-env!)))


(defmacro copy-env-vars! (env vars)
  `(dolist (v ,vars)
     (setenv v (cdr (assoc-string v ,env)))))


(defmacro copy-exec-path-var! ()
  `(progn
     (setenv (shell-env-spec->% :path-var) (shell-env-> :path))
     (when (shell-env-> :exec-path)
       (setq exec-path (shell-env-> :exec-path)))))




;; Darwin/Linux shell 
(platform-supported-unless windows-nt
  
  (when (self-spec->*shell :allowed)
    (load-shell-env!)
    
    (when (self-spec->*shell :bin-path)
      (setenv (shell-env-spec->% :shell-var)
              (self-spec->*shell :bin-path)))
    
    (when (self-spec->*shell :exec-path)
      (copy-exec-path-var!))
    
    (copy-env-vars! (shell-env-> :env-vars)
                    (self-spec->*shell :env-vars))))




;; Windows shell
(platform-supported-when windows-nt
  
  (defadvice ansi-term (before ansi-term-before compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b))))


;; set shell on Windows  
(platform-supported-when windows-nt
  
  (when (file-exists-p (shell-env-spec->% :shell-path))
    (load-shell-env!)
    
    ;; keep `shell-file-name' between `ansi-term' and `shell'
    (path-env<- :shell-file-name shell-file-name)
    
    (defadvice shell (before shell-before compile)
      (setenv (shell-env-spec->% :shell-var) (shell-env-spec->% :shell-path))
      (setenv (shell-env-spec->% :path-var)
              (windows-nt-unix-path (shell-env-> :path)))
      (setq shell-file-name (getenv (shell-env-spec->% :shell-var))))

    (defadvice shell (after shell-after compile)
      (setenv (shell-env-spec->% :shell-var) (shell-env-> :shell-file-name))
      (setenv (shell-env-spec->% :path-var)
              (shell-env-> :path) path-separator)
      (setq shell-file-name (shell-env-> :shell-file-name)))))
