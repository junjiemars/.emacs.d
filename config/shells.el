;;;; -*- lexical-binding:t -*-
;;;;
;; shells
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
  `(self-spec->% (list
		  :source-file
		  ,(concat (v-home% "config/") ".shell-env.el")
		  
		  :compiled-file
		  ,(concat (v-home% "config/") ".shell-env.elc")
		  
		  :shell-var "SHELL"
		  
		  :path-var "PATH"
		  
		  :echo-format
		  ,(platform-supported-if windows-nt
		       "echo %%%s%% 2>/nul"
		     `'((:interactive-shell
			 . "$SHELL -l -i -c 'echo -n $%s' 2>/dev/null")
			(:login-shell
			 . "$SHELL -l -c 'echo -n $%s' 2>/dev/null"))))
     ,@keys))


(defvar *default-shell-env*
  (list :path nil
        :shell-file-name nil
        :exec-path nil
        :env-vars nil)
  "Default shell environments, 
get via `(path-env-> k)' and put via `(path-env<- k v)'")


(defmacro shell-env-> (&optional k)
  "Extract the value from `*default-shell-env*' via K."
  `(if ,k
       (plist-get *default-shell-env* ,k)
     *default-shell-env*))

(defmacro path-env<- (k v)
  "Put K and V into `*default-shell-env*'."
  `(plist-put *default-shell-env* ,k ,v))

(defmacro echo-var (var &optional echo-format)
  "Return the value of $VAR via echo."
  `(shell-command-to-string
    (format (if ,echo-format ,echo-format
              (platform-supported-if windows-nt
                  (shells-spec->% :echo-format)
                (if (self-spec->*env-spec :shell :interactive-shell)
                    (alist-get :interactive-shell
                               (shells-spec->% :echo-format))
                  (alist-get :login-shell
                             (shells-spec->% :echo-format)))))
            ,var)))


(defmacro paths->var (path sep)
  "Convert a list of PATH to $PATH like var that separated by SEP."
  `(string-trim>
    (apply #'concat
           (mapcar #'(lambda (s) (concat s ,sep)) ,path)) ,sep))

(defmacro var->paths (var)
  "Refine VAR like $PATH to list by `path-separator'."
  `(split-string% ,var path-separator t "[ ]+\n"))





(defun save-shell-env! ()
  (path-env<- :path (echo-var (shells-spec->% :path-var)))
  (path-env<- :shell-file-name nil)
  (path-env<- :exec-path (dolist
                             (p (var->paths (shell-env-> :path)) exec-path)
                           (add-to-list 'exec-path p t #'string=)))
  (path-env<- :env-vars (let ((vars (self-spec->*env-spec :shell :env-vars))
                              (x nil))
                          (dolist (v vars x)
                            (push (cons v (echo-var v)) x))))
  (when (save-sexp-to-file
         (list 'setq '*default-shell-env*
               (list 'list
                     ':path (shell-env-> :path)
                     ':shell-file-name nil
                     ':exec-path (list 'quote (shell-env-> :exec-path))
                     ':env-vars (list 'quote (shell-env-> :env-vars))))
         (shells-spec->% :source-file))
    (byte-compile-file (shells-spec->% :source-file))))


(defmacro read-shell-env! ()
  `(progn
     (if (file-exists-p (shells-spec->% :compiled-file))
         (load (shells-spec->% :compiled-file))
       (path-env<- :path (getenv (shells-spec->% :path-var))))
     (add-hook 'kill-emacs-hook #'save-shell-env! t)))


(defmacro copy-env-vars! (env vars)
  `(dolist (v ,vars)
     (setenv v (cdr (assoc-string v ,env)))))


(defmacro copy-exec-path-var! ()
  `(progn
     (setenv (shells-spec->% :path-var) (shell-env-> :path))
     (when (shell-env-> :exec-path)
       (setq exec-path (shell-env-> :exec-path)))))



;; Windows ansi-term
(platform-supported-when windows-nt
  
  (defadvice ansi-term (before ansi-term-before compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b))))





(when (self-spec->*env-spec :shell :allowed)
  (platform-supported-if windows-nt

      ;; shell on Windows-NT 
      (when (file-exists-p (self-spec->*env-spec :shell :bin-path))
	(read-shell-env!)
	
	;; keep `shell-file-name' between `ansi-term' and `shell'
	(path-env<- :shell-file-name shell-file-name)
	
	(defadvice shell (before shell-before compile)
	  (setenv (shells-spec->% :shell-var)
		  (self-spec->*env-spec :shell :bin-path))
	  (setenv (shells-spec->% :path-var)
		  (windows-nt-unix-path (shell-env-> :path)))
	  (setq shell-file-name (getenv (shells-spec->% :shell-var))))

	(defadvice shell (after shell-after compile)
	  (setenv (shells-spec->% :shell-var)
		  (shell-env-> :shell-file-name))
	  (setenv (shells-spec->% :path-var)
		  (shell-env-> :path) path-separator)
	  (setq shell-file-name (shell-env-> :shell-file-name))))

    ;; shell on Darwin/Linux
    (read-shell-env!)
    (when (self-spec->*env-spec :shell :bin-path)
      (setenv (shells-spec->% :shell-var)
	      (self-spec->*env-spec :shell :bin-path)))
    
    (when (self-spec->*env-spec :shell :exec-path)
      (copy-exec-path-var!))
    
    (copy-env-vars! (shell-env-> :env-vars)
		    (self-spec->*env-spec :shell :env-vars))))


(provide 'shells)
