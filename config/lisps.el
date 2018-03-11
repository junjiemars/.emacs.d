;;;; -*- lexical-binding:t -*-
;;;;
;; lisp
;;;;


(defun set-emacs-lisp-mode! ()
  "Set `emacs-lisp-mode'."
  (eldoc-mode)
  (cond
   ((string= "*scratch*" (buffer-name))
		(when-fn% disable-paredit-mode paredit
			(disable-paredit-mode)))))


(defun set-ielm-mode! ()
	"Set `ielm-mode'."
	(when-fn% disable-paredit-mode paredit
		(disable-paredit-mode)))


(defun set-lisp-basic-mode! ()
	"Set basic minor modes."
  ;; structured editing of s-expression data
  (enable-paredit-mode)
  ;; enable automatically adjust the identation of code
  (aggressive-indent-mode)
  ;; hilighting parentheses,brackets,and braces in minor mode
  (rainbow-delimiters-mode))


(provide 'lisps)
