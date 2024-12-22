;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser-autoload.el
;;;;

(autoload 'use-geiser-init! (v-home%> "config/use-geiser"))

;; disable auto active `geiser-mode'
(fset 'geiser-mode--maybe-activate #'true)

;; disable auto `geiser-mode' for `scheme-mode'
(set-default 'geiser-mode-auto-p nil)

;; `geiser' after load
(with-eval-after-load 'geiser-mode
  (use-geiser-init!))

;; end of use-geiser-autoload.el
