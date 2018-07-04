;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;


(with-eval-after-load 'dired
	
	(platform-supported-when
			darwin
		;; on Drawin: ls does not support --dired option
		;; see `dired-use-ls-dired' for more defails
		(setq% dired-use-ls-dired nil dired))

	(platform-supported-when
			windows-nt
		;; on Windows: there are no builtin zip program
		;; so try to use minzip in Emacs dep for Windows.
		;; see `dired-do-compress-to'.
		(when-var% dired-compress-files-alist dired-aux
							 (unless (executable-find% "zip")
								 (require 'dired-aux)
								 (let ((zip (assoc "\\.zip\\'" dired-compress-files-alist)))
									 (when (and zip (string-match-p "^zip" (cdr zip))
															(executable-find% "minizip"))
										 (setq dired-compress-files-alist
													 (append (remove zip dired-compress-files-alist)
																	 '(("\\.zip\\'" . "minizip %o -9 %i"))))))))

		(when (executable-find% "ls")
			;; on Windows: `dired-mode' does not display executable flag in file mode
			;; when GNU's ls program already in PATH.
			(setq% ls-lisp-use-insert-directory-program t ls-lisp))))
