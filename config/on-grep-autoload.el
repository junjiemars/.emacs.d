;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-grep-autoload.el
;;;;




(platform-supported-when windows-nt

	(with-eval-after-load 'grep
		;; using `find-dired' on Windows
		(let ((find (executable-find* "find" "\\\\usr\\\\bin\\\\")))
			(when find
				(setq% find-program find grep)))))
