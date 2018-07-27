;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider.el
;;;;


(feature-paredit-supported-p
  
  (defun set-clojure-mode! ()
    "Hook into `clojure-mode-hook'"
    (enable-paredit-mode)
    (subword-mode)
    (rainbow-delimiters-mode)
    ;; (aggressive-indent-mode)
    ))

(feature-paredit-supported-p
  
  (defun set-cider-repl-mode! ()
    "Hook into `cider-repl-mode-hook'"
    (eldoc-mode)
    (enable-paredit-mode)
    (rainbow-delimiters-mode)
    ;; (aggressive-indent-mode)
    ))


(defun use-clojure-mode ()
  (feature-paredit-supported-p
    (package-spec-:allowed-p
      (add-hook 'clojure-mode-hook #'set-clojure-mode!))))


(defun use-cider-repl ()
  "Use cide repl."
  
  (require 'use-cider)
  
  ;; Where to store the cider history.
  (setq% cider-repl-history-file
	 (v-home! ".cider/" "repl-history") cider-repl)
  ;; Wrap when navigating history.
  (setq% cider-repl-wrap-history t cider)
  
  ;; Go right to the REPL buffer when it's finished connecting
  (setq% cider-repl-pop-to-buffer-on-connect t cider)
  
  ;; When there's a cider error, show its buffer and switch to it
  (setq% cider-show-error-buffer t cider)
  (setq% cider-auto-select-error-buffer t cider)
  
  (feature-paredit-supported-p
    (package-spec-:allowed-p
      (add-hook 'cider-repl-mode-hook #'set-cider-repl-mode!))))


;; It's a bug in cider/clojure
(when-fn% org-bookmark-jump-unhide org
  (declare-function org-bookmark-jump-unhide "org"))


(provide 'use-cider)
