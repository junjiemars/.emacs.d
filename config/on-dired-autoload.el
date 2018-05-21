;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;


;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails

(with-eval-after-load 'dired
	(platform-supported-when
			darwin
		(setq% dired-use-ls-dired nil dired))

	(platform-supported-when
			windows-nt
		(when-var%
		 dired-compress-files-alist dired-aux
		 (unless (executable-find "zip")
			 (let ((zip (assoc "\\.zip\\'" dired-compress-files-alist)))
				 (when (and zip (executable-find "minizip"))
					 (setq dired-compress-files-alist
								 (append (remove zip dired-compress-files-alist)
												 '(("\\zip\\'" . "minizip %o -9 %i"))))))))))
