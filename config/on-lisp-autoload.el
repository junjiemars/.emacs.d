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
	(package-spec-:allowed-p

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
								#'enable-paredit-mode-in-minibuffer! t))))
