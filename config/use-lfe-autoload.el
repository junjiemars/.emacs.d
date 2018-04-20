;;;; -*- lexical-binding:t -*-
;;;;
;; use-lfe (Lisp Flavoured Erlang)
;;;;


(with-eval-after-load 'lfe-mode
	
	;; Enable paredit for LFE
	(add-hook 'lfe-mode-hook #'enable-paredit-mode)

	;; For .lfe files
	(add-to-list 'auto-mode-alist '("\\.lfe\\'" . lfe-mode)))



