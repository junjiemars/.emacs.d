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
			 (list :source-file ,(v-home! ".exec/.shell-env.el")
						 :compiled-file ,(v-home! ".exec/.shell-env.elc")
						 :SHELL "SHELL"
						 :PATH "PATH")
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
							 (dolist (p (append (var->paths (getenv (shells-spec->% :PATH)))
																	(platform-supported-unless windows-nt
																		(list (v-home% ".exec/")))) exec-path)
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


(defmacro copy-exec-path! (path)
  `(when ,path (setq exec-path ,path)))




;; Windows ansi-term/shell

(platform-supported-when windows-nt
  
  (defadvice ansi-term (before ansi-term-before compile)
    (set-window-buffer (selected-window)
											 (make-comint-in-buffer "ansi-term" nil "cmd"))))


(platform-supported-when windows-nt
  (with-eval-after-load 'term (ad-activate #'ansi-term t)))


(platform-supported-when windows-nt

	(defun windows-nt-env-path+ (dir &optional append)
		"APPEND or push DIR to %PATH%."
		(let ((env (var->paths (getenv (shells-spec->% :PATH)))))
			(when (or (and (null append) (not (string= dir (first env))))
								(and append (not (string= dir (last env)))))
				(eval-when-compile (require 'cl))
				(let ((path (with-no-warnings
											(require 'cl)
											(remove* dir env :test #'string=))))
					(setenv (shells-spec->% :PATH)
									(paths->var (if append
																	(append path dir)
																(cons dir path)))))))))

 ;; end of Windows ansi-term/shell


;; allowed/disallowed `shells-spec->*'

(if (shells-spec->* :allowed)
		(progn
			(read-shell-env!)
			
			(when (shells-spec->* :shell-file-name)
				(setenv (shells-spec->% :SHELL)
								(shells-spec->* :shell-file-name)))
			
			(when (shells-spec->* :exec-path)
				(copy-exec-path! (shell-env-> :exec-path)))

			(when (shells-spec->* :env-vars)
				(copy-env-vars! (shell-env-> :env-vars)
												(shells-spec->* :env-vars))))
	;; disallowed: append .exec/ to `exec-path'
	(add-to-list 'exec-path (v-home% ".exec/") t #'string=))


 ;; end of allowed/disallowed `shells-spec->*'


(platform-supported-when windows-nt
	
	(defun make-zip-bat (zip &rest ignore)
		"Make ZIP.bat in `exec-path."
		(declare (indent 1))
		(when (stringp zip)
			(save-str-to-file
			 (concat "@echo off\n"
							 (format "REM zip.bat for %s on Windows\n" zip)
							 "REM generated by More Reasonable Emacs https://github.com/junjiemars/.emacs.d\n\n"
							 (concat "REM local variable declaration\n\n"
											 "setlocal EnableDelayedExpansion\n"
											 "\n"
											 "set _OPT=%*\n"
											 "set _ZIP=\n"
											 "set _ARGV=\n"
											 "\n"
											 "REM parsing command line arguments\n\n"
											 ":getopt\n"
											 (cond ((string= "minizip" zip)
															"if \"%1\"==\"-mX0\" set _OPT=%_OPT:-mX0=-0% & shift & goto :getopt\n")
														 ((string= "7za" zip)
															(concat
															 "if \"%1\"==\"-mX0\" set _OPT=%_OPT:-mX0=-mx0% & shift & goto :getopt\n"
															 "if \"%1\"==\"-0\" set _OPT=%_OPT:-0=-mx0% & shift & goto :getopt\n"
															 "if \"%1\"==\"-9\" set _OPT=%_OPT:-9=-mx9% & shift & goto :getopt\n")))
											 "\n"
											 "REM ignore options\n"
											 (let ((options nil))
												 (dolist (x (cond ((string= "minizip" zip)
																					 (append '("-r" "--filesync" "-rmTq") ignore))
																					((string= "7za" zip)
																					 (append '("-r" "--filesync" "-rmTq"))))
																		options)
													 (setq options
																 (concat options
																				 (format "if \"%%1\"==\"%s\" set _OPT=%%_OPT:%s=%% & shift & goto :getopt\n" x x)))))
											 "\n"
											 "REM extract zip and argv\n"
											 "if not \"%1\"==\"\" (\n"
											 "  if \"%_ZIP%\"==\"\" (\n"
											 "    if \"%_ARGV%\"==\"\" (\n"
											 "      set _ZIP=%1\n"
											 "    )\n"
											 "  ) else (\n"
											 "    set _ARGV=%_ARGV% %1\n"
											 "  )\n"
											 "  set _OPT=!_OPT:%1=!\n"
											 "  shift\n"
											 "  goto :getopt\n"
											 ")\n\n")
							 (cond ((string= "7za" zip)
											(concat "REM 7za call\n"
															"7za a %_OPT% -tzip -- %_ZIP% %_ARGV%\n"
															"if exist %_ZIP% (\n"
															"  7za d %_OPT% -tzip -- %_ZIP% %_ZIP%\n"
															")\n"))
										 ((string= "minizip" zip)
											(concat "REM minizip recursive call\n\n"
															"call :loop %_ARGV%\n"
															"goto :end\n"
															"\n:zip\n"
															"set _file=%1\n"
															"set _file=%_file:./=%\n"
															"if not \"%_file%\"==\"%_ZIP%\" (\n"
															"  if exist %_ZIP% (\n"
															"    minizip %_OPT% -a %_ZIP% %_file%\n"
															"  ) else (\n"
															"    minizip %_OPT% %_ZIP% %_file%\n"
															"  )\n"
															")\n"
															"goto :end\n"
															"\n:loop\n"
															"for %%i in (%*) do (\n"
															"  if exist \"%%i/*\" (\n"
															"    for %%f in (%%i/*) do (\n"
															"      call :loop %%i/%%f\n"
															"    )\n"
															"    for /d %%d in (%%i/*) do (\n"
															"      call :loop %%i/%%d\n"
															"    )\n"
															"  ) else (\n"
															"    call :zip %%i\n"
															"  )\n"
															")\n"
															"\n:end\n"))))
			 (v-home% ".exec/zip.bat")))))


 ;; end of shells.el
