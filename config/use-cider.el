;;;; -*- lexical-binding:t -*-
;;;;
;; use-cider
;;;;


;;;;
;; Clojure
;;;;


(defun set-clojure-mode! ()
  (enable-paredit-mode)
  (subword-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))
;; clojure mode hooks
(add-hook 'clojure-mode-hook #'set-clojure-mode!)


;; use clojure mode for other extensions
;; (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'magic-mode-alist '(".* boot" . clojure-mode))


;;;;
;; Cider
;;;;


;; Go right to the REPL buffer when it's finished connecting
(safe-setq cider-repl-pop-to-buffer-on-connect t)


;; When there's a cider error, show its buffer and switch to it
(safe-setq cider-show-error-buffer t)
(safe-setq cider-auto-select-error-buffer t)


;; Where to store the cider history.
(safe-setq cider-repl-history-file (v-home! ".cider-history/"))


;; Wrap when navigating history.
(safe-setq cider-repl-wrap-history t)



(provide 'use-cider)
