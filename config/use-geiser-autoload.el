;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser-autoload.el
;;;;

(declare-function use-geiser-init! (v-home%> "config/use-geiser"))
(autoload 'use-geiser-init! (v-home%> "config/use-geiser"))

;;; Disable auto active `geiser-mode'
(fset 'geiser-mode--maybe-activate nil)

;;; Disable auto `geiser-mode' for `scheme-mode'
(when-var% geiser-mode-auto-p 'geiser-mode
  (setq% geiser-mode-auto-p nil 'geiser-mode))

;; `geiser' after load
(with-eval-after-load 'geiser
  (use-geiser-init!))

;; end of use-geiser-autoload.el
