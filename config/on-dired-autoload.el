;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;


;; on Drawin: ls does not support --dired;
;; see `dired-use-ls-dired' for more defails
(platform-supported-when
		darwin
	(with-eval-after-load 'dired
		(setq% ls-lisp-use-insert-directory-program nil)))
