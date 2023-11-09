;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider-autoload.el
;;;;


(defun-on-module-autoload^ cider
  ;; use clojure mode for other extensions
  ;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (push! '("\\.boot$" . clojure-mode) auto-mode-alist )
  (push! '(".* boot" . clojure-mode) magic-mode-alist)

  (declare-function use-clojure-mode (v-home%> "config/use-cider.el"))
  (declare-function use-cider-repl (v-home%> "config/use-cider.el"))

  (with-eval-after-load 'clojure-mode
    (use-clojure-mode))

  (with-eval-after-load 'cider-repl
    (use-cider-repl)))


(add-hook 'after-init-hook #'on-cider-autoload^ t)
