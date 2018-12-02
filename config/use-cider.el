;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider.el
;;;;


(defun set-clojure-mode! ()
  "Hook into `clojure-mode-hook'"
  (subword-mode)
  (feature-allowed-p paredit (enable-paredit-mode))
  (feature-allowed-p rainbow-delimiters (rainbow-delimiters-mode))
  (feature-allowed-p aggressive-indent (aggressive-indent-mode)))


(defun set-cider-repl-mode! ()
  "Hook into `cider-repl-mode-hook'"
  (eldoc-mode)
  (feature-allowed-p paredit (enable-paredit-mode))
  (feature-allowed-p rainbow-delimiters (rainbow-delimiters-mode))
  (feature-allowed-p aggressive-indent (aggressive-indent-mode)))


(defun use-clojure-mode ()
  (add-hook 'clojure-mode-hook #'set-clojure-mode!))


(defun use-cider-repl ()
  "Use cide repl."
  
  (require 'use-cider)
  
  ;; Where to store the cider history.
  (setq% cider-repl-history-file
         (v-home! ".cider/repl-history") cider-repl)
  ;; Wrap when navigating history.
  (setq% cider-repl-wrap-history t cider)
  
  ;; Go right to the REPL buffer when it's finished connecting
  (setq% cider-repl-pop-to-buffer-on-connect t cider)
  
  ;; When there's a cider error, show its buffer and switch to it
  (setq% cider-show-error-buffer t cider)
  (setq% cider-auto-select-error-buffer t cider)
  
  (add-hook 'cider-repl-mode-hook #'set-cider-repl-mode!))


;; It's a bug in cider/clojure
(when-fn% 'org-bookmark-jump-unhide 'org
  (declare-function org-bookmark-jump-unhide "org"))


(provide 'use-cider)
