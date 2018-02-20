;;;; -*- lexical-binding:t -*-
;;;;
;; Semantics
;;;;


;; (defun c-turn-on-eldoc-mode ()
;;   "Enable c-eldoc-mode"
;;   (interactive)
;;   (add-function :before-until
;;                 (local 'eldoc-documentation-function)
;;                 #'c-eldoc-semantic-ia-show-summary)
;;   (enable-eldoc-mode))

;; (defun c-eldoc-semantic-ia-show-summary ()
;;   (interactive)
;;   (semantic-ia-show-summary (point)))

;; (when semantic-mode
;;   (add-hook 'c-mode-hook #'c-turn-on-eldoc-mode))

