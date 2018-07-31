;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;


(if-fn% dired-do-compress-to dired-aux
				(platform-supported-when windows-nt
					;; on Windows: there are no builtin zip program
					;; so try to use minzip in Emacs dep for Windows.
					;; see `dired-do-compress-to'.
					(when-var% dired-compress-files-alist dired-aux
										 (with-eval-after-load 'dired-aux
											 (unless (executable-find% "zip")
												 ;; `format-spec' may not autoload
												 (require 'format-spec)
												 (let ((zip (assoc** "\\.zip\\'" dired-compress-files-alist #'string=)))
													 (when (and zip (string-match-p "^zip" (cdr zip))
																			(executable-find% "minizip"))
														 (setcdr zip "minizip %o -9 %i")))))))
	(when-var% dired-compress-file-suffixes dired-aux
						 ;; on ancent Emacs, `dired' can't recognize .zip archive.
						 ;; [Z] key should be recognize .zip extension and uncompress a .zip archive.
						 ;; [! zip x.zip ?] compress marked files to x.zip
						 ;; see `dired-compress-file-suffixes'.
						 (with-eval-after-load 'dired-aux
							 (when (and (executable-find% "zip")
													(executable-find% "unzip"))
								 (unless (assoc** "\\.zip\\'" dired-compress-file-suffixes #'string=)
									 (add-to-list 'dired-compress-file-suffixes
																'("\\.zip\\'" ".zip" "unzip")))))))


(platform-supported-unless gnu/linux

	(with-eval-after-load 'ido
		;; see `ido-dired'
		(if (executable-find% "ls"
													(lambda (ls)
														(let ((ver (shell-command* ls "--version")))
															(when (zerop (car ver))
																(string-match "^ls (GNU coreutils)"
																							(cdr ver))))))
				;; prefer GNU's ls on Windows or Darwin
				;; on Windows: `dired-mode' does not display executable flag in file mode
				;; see `dired-use-ls-dired' for more defails
				(setq% ls-lisp-use-insert-directory-program t ls-lisp)
			(platform-supported-when darwin
				;; on Drawin: ls does not support --dired option
				(setq% dired-use-ls-dired nil dired)))))


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)


