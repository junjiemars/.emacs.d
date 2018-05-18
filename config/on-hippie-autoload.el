;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-hippie-autoload.el
;;;;


;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
			'(try-expand-dabbrev
				try-expand-dabbrev-all-buffers
				try-expand-dabbrev-from-kill
				try-complete-lisp-symbol-partially
				try-complete-lisp-symbol))


;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)




