;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lfe-autoload.el (Lisp Flavoured Erlang)
;;;;


(with-eval-after-load 'lfe-mode
	
	;; Enable paredit for LFE
	(add-hook 'lfe-mode-hook #'enable-paredit-mode)

	;; For .lfe files
	(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode)))



