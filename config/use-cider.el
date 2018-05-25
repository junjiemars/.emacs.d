;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider.el
;;;;


;;;;
;; Clojure
;;;;


(defun use-clojure-mode! ()
	"Hook into `clojure-mode-hook'"
  (enable-paredit-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


;;;;
;; Cider
;;;;


(defun use-cider-repl-mode! ()
  "Hook into `cider-repl-mode-hook'"
  (eldoc-mode)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


;; If there are no Java env, but try to load `use-cider'
;; will trigger a comile-warning
;; check :shell settings in `def-self-env-spec'
;; (declare-function org-bookmark-jump-unhide "org")


(provide 'use-cider)
