;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider-autoload.el
;;;;


(defun-on-module-autoload^ cider
  ;; use clojure mode for other extensions
  ;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


  (autoload 'use-clojure-mode
    (v-home% "config/use-cider.elc"))

  (autoload 'use-cider-repl
    (v-home% "config/use-cider.elc"))


  (with-eval-after-load 'clojure-mode
    (use-clojure-mode))


  (with-eval-after-load 'cider-repl
    (use-cider-repl)))


(add-hook 'after-init-hook #'on-cider-autoload^ t)
