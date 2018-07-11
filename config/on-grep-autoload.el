;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-grep-autoload.el
;;;;




(platform-supported-when windows-nt

	(with-eval-after-load 'grep
		;; prefer GNU find on Windows, such for `find-dired' or `find-name-dired'.
		(let ((find (executable-find*
								 "find"
								 (lambda (bin)
									 (string-match "^find (GNU findutils)"
																 (shell-command-to-string
																	(concat bin " --version")))))))
			(when find (setq% find-program find grep)))))
