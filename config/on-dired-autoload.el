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
		(setq% dired-use-ls-dired nil dired)))
