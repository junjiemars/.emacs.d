;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider-autoload.el
;;;;



(autoload 'use-clojure-mode
  (v-home% "config/" "use-cider.elc"))

(autoload 'use-cider-repl
  (v-home% "config/" "use-cider.elc"))


;; use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


(with-eval-after-load 'clojure-mode
	(use-clojure-mode))


(with-eval-after-load 'cider-repl
	(use-cider-repl))
