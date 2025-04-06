;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit-autoload.el
;;;;

(autoload 'use-magit-init! (v-home%> "config/use-magit"))

;; toggle off `magit-auto-revert-mode' in `magit-autorevert'
(set-default 'magit-auto-revert-mode nil)

(eval-when-compile
  (when-feature% transient
    ;; avoid lower `transient-version' issue
    (unload-feature 'transient t)
    (require 'transient)))

;;; add `magit' into `vc*-frontend'
(add-hook 'vc*-frontend-hook `("magit" . magit-status))

;;; `magit' after load
(with-eval-after-load 'magit
  (make-thread* #'use-magit-init!))

;; end of use-magit-autoload.el
