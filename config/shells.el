;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
  `(self-spec->%
			 (list :source-file
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

(defmacro shell-env<- (k v)
  "Put K and V into `*default-shell-env*'."
  `(plist-put *default-shell-env* ,k ,v))

(defmacro echo-var (var shell options)
  "Return the value of $VAR via echo."
  `(when (and (stringp ,var) (stringp ,shell))
		 (let ((cmd (shell-command* (shell-quote-argument ,shell)
									(and (consp ,options)
											 (mapconcat #'identity ,options " "))
									(format "-c 'echo $%s'" ,var))))
			 (when (zerop (car cmd))
				 (string-trim> (cdr cmd))))))


(defmacro paths->var (path sep)
  "Convert a list of PATH to $PATH like var that separated by SEP."
  `(string-trim>
		(apply #'concat
					 (mapcar #'(lambda (s) (concat s ,sep)) ,path))
		,sep))

(defmacro var->paths (var)
  "Refine VAR like $PATH to list by `path-separator'."
  `(when (stringp ,var)
		 (split-string* ,var path-separator t "[ ]+\n")))



(defun save-shell-env! ()
  (shell-env<- :path (echo-var (shells-spec->% :path-var)
															 (self-spec->*env-spec :shell :bin-path)
															 (self-spec->*env-spec :shell :interactive-shell)))
  (shell-env<- :shell-file-name nil)
  (shell-env<- :exec-path
							 (dolist (p (var->paths (shell-env-> :path)) exec-path)
								 (when (stringp p) (add-to-list 'exec-path p t #'string=))))
  (shell-env<- :env-vars
							 (let ((vars (self-spec->*env-spec :shell :env-vars))
										 (x nil))
								 (dolist (v vars x)
									 (let ((v1 (echo-var v
																			 (self-spec->*env-spec :shell :bin-path)
																			 (self-spec->*env-spec :shell :interactive-shell))))
										 (when v1 (push (cons v v1) x))))))
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
       (shell-env<- :path (getenv (shells-spec->% :path-var))))
     (add-hook 'kill-emacs-hook #'save-shell-env! t)))


(defmacro copy-env-vars! (env vars)
  `(dolist (v ,vars)
		 (when v (let ((v1 (alist-get* v ,env nil nil #'string=)))
							 (when v1 (setenv v v1))))))


(defmacro copy-exec-path-var! ()
  `(progn
		 (when (shell-env-> :path)
			 (setenv (shells-spec->% :path-var) (shell-env-> :path)))
     (when (shell-env-> :exec-path)
       (setq exec-path (shell-env-> :exec-path)))))




;; Windows ansi-term and shell

(platform-supported-when windows-nt
  
  (defadvice ansi-term (before ansi-term-before compile)
    (let* ((n "*ansi-term*")
           (b (get-buffer-create n)))
      (apply 'make-comint-in-buffer n b "cmd" nil nil)
      (set-window-buffer (selected-window) b))))

(platform-supported-when windows-nt

  (defadvice shell (before shell-before compile)
    (setenv (shells-spec->% :shell-var)
	    (self-spec->*env-spec :shell :bin-path))
    (setenv (shells-spec->% :path-var)
	    (windows-nt-unix-path (shell-env-> :path)))
    (setq shell-file-name (getenv (shells-spec->% :shell-var)))))

(platform-supported-when windows-nt
  
  (defadvice shell (after shell-after compile)
    (setenv (shells-spec->% :shell-var)
	    (shell-env-> :shell-file-name))
    (setenv (shells-spec->% :path-var)
	    (shell-env-> :path) path-separator)
    (setq shell-file-name (shell-env-> :shell-file-name))))


(platform-supported-when windows-nt
  (with-eval-after-load 'term (ad-activate #'ansi-term t)))



;; add versioned `+emacs-exec-home+' to $PATH
(env-path+ +emacs-exec-home+)

(when (self-spec->*env-spec :shell :allowed)
  (platform-supported-if windows-nt

      ;; shell on Windows-NT 
      (when (file-exists-p (self-spec->*env-spec :shell :bin-path))
				(read-shell-env!)
				
				;; keep `shell-file-name' between `ansi-term' and `shell'
				(shell-env<- :shell-file-name shell-file-name)
				(with-eval-after-load 'shell (ad-activate #'shell t)))

    ;; shell on Darwin/Linux
    (read-shell-env!)
    (when (self-spec->*env-spec :shell :bin-path)
      (setenv (shells-spec->% :shell-var)
							(self-spec->*env-spec :shell :bin-path)))
    
    (when (self-spec->*env-spec :shell :exec-path)
      (copy-exec-path-var!))
    
    (copy-env-vars! (shell-env-> :env-vars)
										(self-spec->*env-spec :shell :env-vars))))

