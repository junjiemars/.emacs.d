;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shells.el
;;;;


(defmacro shells-spec->% (&rest keys)
  "Extract value from the list of spec via KEYS at compile time."
	(declare (indent 0))
  `(self-spec->%
			 (list :source-file
						 ,(concat +emacs-exec-home+ ".shell-env.el")
						 
						 :compiled-file
						 ,(concat +emacs-exec-home+ ".shell-env.elc")
						 
						 :shell-var "SHELL"
						 
						 :path-var "PATH")
     ,@keys))


(defmacro shells-spec->* (&rest keys)
	"Extract value from the list of :shell spec via KEYS at runtime."
	(declare (indent 0))
	 `(self-spec->*env-spec :shell ,@keys))


(defvar *default-shell-env*
  (list :exec-path nil
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
		 (let ((cmd (shell-command* (shell-quote-argument (or ,shell
																													(getenv "SHELL")))
									(mapconcat #'identity ,options " ")
									(format "-c 'echo $%s'" ,var))))
			 (when (zerop (car cmd))
				 (string-trim> (cdr cmd))))))


(defmacro paths->var (path)
  "Convert a list of PATH to $PATH like var that separated by SEP."
  `(string-trim> (apply #'concat
												(mapcar #'(lambda (s) (concat s path-separator)) ,path))
								 path-separator))

(defmacro var->paths (var)
  "Refine VAR like $PATH to list by `path-separator'."
  `(when (stringp ,var)
		 (split-string* ,var path-separator t "[ ]+\n")))



(defun save-shell-env! ()
  (shell-env<- :exec-path
							 (dolist (p (var->paths (shell-env-> :path)) exec-path)
								 (when (stringp p) (add-to-list 'exec-path p t #'string=))))
  (shell-env<- :env-vars
							 (let ((vars (shells-spec->* :env-vars))
										 (x nil))
								 (dolist (v vars x)
									 (let ((v1 (echo-var v
																			 (shells-spec->* :shell-file-name)
																			 (shells-spec->* :options))))
										 (when v1 (push (cons v v1) x))))))
  (when (save-sexp-to-file
         (list 'setq '*default-shell-env*
               (list 'list
                     ':exec-path (list 'quote (shell-env-> :exec-path))
                     ':env-vars (list 'quote (shell-env-> :env-vars))))
         (shells-spec->% :source-file))
    (byte-compile-file (shells-spec->% :source-file))))


(defmacro read-shell-env! ()
  `(progn
     (when (file-exists-p (shells-spec->% :compiled-file))
			 (load (shells-spec->% :compiled-file)))
     (add-hook 'kill-emacs-hook #'save-shell-env! t)))


(defmacro copy-env-vars! (env vars)
  `(dolist (v ,vars)
		 (when v (let ((v1 (alist-get* v ,env nil nil #'string=)))
							 (when v1 (setenv v v1))))))


(defmacro copy-exec-path-var! (path)
  `(when ,path (setq exec-path ,path)))




;; Windows ansi-term and shell

(platform-supported-when windows-nt
  
  (defadvice ansi-term (before ansi-term-before compile)
    (set-window-buffer (selected-window)
											 (make-comint-in-buffer "ansi-term" nil "cmd"))))


(platform-supported-when windows-nt
  (with-eval-after-load 'term (ad-activate #'ansi-term t)))


(platform-supported-when windows-nt

	(defsubst windows-nt-env-path+ (dir &optional append)
		"APPEND or insert DIR into %PATH%."
		(let ((env (var->paths (getenv (shells-spec->% :path-var)))))
			(when (or (and (null append) (not (string= dir (first env))))
								(and append (not (string= dir (last env)))))
				(eval-when-compile (require 'cl))
				(let ((path (with-no-warnings
											(require 'cl)
											(remove* dir env :test #'string=))))
					(setenv (shells-spec->% :path-var)
									(paths->var (if append
																	(append path dir)
																(cons dir path)))))))))




(when (shells-spec->* :allowed)

	(read-shell-env!)
	
  (when (shells-spec->* :shell-file-name)
    (setenv (shells-spec->% :shell-var)
						(shells-spec->* :shell-file-name)))
  
  (when (shells-spec->* :exec-path)
    (copy-exec-path-var! (shell-env-> :exec-path)))

  (when (shells-spec->* :env-vars)
		(copy-env-vars! (shell-env-> :env-vars)
										(shells-spec->* :env-vars))))


;; append versioned `+emacs-exec-home+' to $PATH
(setenv (shells-spec->% :path-var)
				(path+ (getenv (shells-spec->% :path-var)) t +emacs-exec-home+))


 ;; end of shells.el
