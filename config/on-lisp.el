;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp.el
;;;;


(defun set-lisp-basic-mode! ()
	"Set Lisp basic minor modes."
	(cond ((or (string= "*scratch*" (buffer-name))
						 (string= "*ielm*" (buffer-name))))
				(t
				 ;; structured editing of s-expression data
				 (enable-paredit-mode)
				 ;; enable automatically adjust the identation of code
				 (aggressive-indent-mode)
				 ;; hilighting parentheses,brackets,and braces in minor mode
				 (rainbow-delimiters-mode))))


(defun enable-paredit-mode-in-minibuffer! ()
	(platform-supported-if
			gnu/linux
			(when (eq 'eval-expression this-command)
				(enable-paredit-mode))
		(enable-paredit-mode)))


(provide 'on-lisp)
