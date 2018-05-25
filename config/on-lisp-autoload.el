;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
;;;;


(with-eval-after-load 'lisp-mode
	(safe-local-variable* 'Syntax)
	(safe-local-variable* 'Base)
	(safe-local-variable* 'Package))


(version-supported-if
		<= 25.0
		(with-eval-after-load 'elisp-mode
			(add-hook 'emacs-lisp-mode-hook #'eldoc-mode))
	(with-eval-after-load 'lisp-mode
		(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))


(with-eval-after-load 'ielm
	(add-hook 'ielm-mode-hook #'eldoc-mode))


(feature-paredit-supported-p
	
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
					 (rainbow-delimiters-mode)))))


(feature-paredit-supported-p
	
	(defun enable-paredit-mode-in-minibuffer! ()
		(platform-supported-if
				gnu/linux
				(when (eq 'eval-expression this-command)
					(enable-paredit-mode))
			(enable-paredit-mode))))


(feature-allowed-p paredit

	(with-eval-after-load 'lisp-mode
		(add-hook 'lisp-mode-hook #'set-lisp-basic-mode!)
		(add-hook 'emacs-lisp-mode-hook #'set-lisp-basic-mode!))

	(with-eval-after-load 'scheme
		(add-hook 'scheme-mode-hook #'set-lisp-basic-mode!))

	(platform-supported-if
			gnu/linux
			(add-hook 'minibuffer-setup-hook
								#'enable-paredit-mode-in-minibuffer! t)
		(add-hook 'eval-expression-minibuffer-setup-hook
							#'enable-paredit-mode-in-minibuffer! t))

	
	;; `paredit' keymap
	;; On Windows C-) is not work
	;; fix inconsistent `C-)' `C-c )' behavior:#9
	
	(global-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)

	;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
	
	(terminal-supported-p
		(global-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
		(global-set-key (kbd "C-c }") 'paredit-forward-barf-sexp)
		(global-set-key (kbd "C-c {") 'paredit-backward-barf-sexp)))

 ;; end of package: paredit
