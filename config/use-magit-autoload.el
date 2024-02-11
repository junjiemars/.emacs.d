;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit-autoload.el
;;;;

(declare-function use-magit-init! (v-home%> "config/use-magit"))
(autoload 'use-magit-init! (v-home%> "config/use-magit"))

;;; toggle off `magit-auto-revert-mode'
(setq% magit-auto-revert-mode nil 'magit-autorevert)

;;; `magit' after load
(with-eval-after-load 'magit
  (use-magit-init!))

;; end of use-magit-autoload.el
