;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-grep-autoload.el
;;;;




(platform-supported-when windows-nt
	;; use `find-dired' on Windows
	(with-eval-after-load 'grep
		(let ((find (executable-find* "find" "\\\\usr\\\\bin\\\\")))
			(when find
				(setq find-program find)))))
