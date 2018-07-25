;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;



(with-eval-after-load 'dired-aux

	(unless-fn% dired-do-compress-to dired-aux
		;; on ancent Emacs, `dired' can't recognize .zip archive.
		;; [Z] key should be recognize .zip extension and uncompress a .zip archive.
		;; [! zip x.zip ?] compress marked files to x.zip
		(when (and (executable-find% "zip")
							 (executable-find% "unzip"))
			(when-var%
			 dired-compress-file-suffixes dired-aux
			 (unless (assoc** "\\.zip\\'" dired-compress-file-suffixes #'string=)
				 (add-to-list 'dired-compress-file-suffixes
											'("\\.zip\\'" ".zip" "unzip"))))))

	(platform-supported-when
      windows-nt
    ;; on Windows: there are no builtin zip program
    ;; so try to use minzip in Emacs dep for Windows.
    ;; see `dired-do-compress-to'.
    (when-var%
		 dired-compress-files-alist dired-aux
		 (unless (executable-find% "zip")
			 (require 'dired-aux)
			 (let ((zip (assoc** "\\.zip\\'" dired-compress-files-alist #'string=)))
				 (when (and zip (string-match-p "^zip" (cdr zip))
										(executable-find% "minizip"))
					 (setcdr zip "minizip %o -9 %i")))))

    (when (executable-find%
					 "ls"
					 (lambda (ls)
						 (string-match "^ls (GNU coreutils)"
													 (shell-command-to-string
														(concat ls " --version")))))
      ;; prefer GNU's ls on Windows
      ;; on Windows: `dired-mode' does not display executable flag in file mode
      (setq% ls-lisp-use-insert-directory-program t ls-lisp))))


(with-eval-after-load 'dired

  (platform-supported-when
      darwin
    ;; on Drawin: ls does not support --dired option
    ;; see `dired-use-ls-dired' for more defails
    (setq% dired-use-ls-dired nil dired)))
