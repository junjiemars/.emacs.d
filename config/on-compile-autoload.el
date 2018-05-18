;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-compile-autoload.el
;;;;


(with-eval-after-load 'compile
	(add-hook 'compilation-filter-hook #'colorize-compilation-buffer!)
	(define-key* compilation-mode-map (kbd "g") #'recompile compile)
	(define-key* compilation-mode-map (kbd "q") #'quit-window compile)
	(setq% compilation-scroll-output t compile))
