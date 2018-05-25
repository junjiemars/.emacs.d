;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider-autoload.el
;;;;



(autoload 'use-clojure-mode!
  (v-home% "config/" "use-cider.elc"))

(autoload 'use-cider-repl-mode!
  (v-home% "config/" "use-cider.elc"))


;; use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


(with-eval-after-load 'clojure-mode
	(require 'use-cider)
	(add-hook 'clojure-mode-hook #'use-clojure-mode!))


(with-eval-after-load 'cider-repl
	
	;; Where to store the cider history.
	(setq% cider-repl-history-file
				 (v-home! ".cider-history/" "repl") cider-repl)

	;; Wrap when navigating history.
	(setq% cider-repl-wrap-history t cider)

	;; Go right to the REPL buffer when it's finished connecting
	(setq% cider-repl-pop-to-buffer-on-connect t cider)

	;; When there's a cider error, show its buffer and switch to it
	(setq% cider-show-error-buffer t cider)
	(setq% cider-auto-select-error-buffer t cider)

	(add-hook 'cider-repl-mode-hook #'use-cider-repl-mode!))
