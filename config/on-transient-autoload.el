;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-transient-autoload.el
;;;;

(autoload 'on-transient-init! (v-home%> "config/transients"))

(when-feature-transient%
  (with-eval-after-load 'transient
    (make-thread* #'on-transient-init!)))


;;; `transient-mark-mode'
(unless-graphic%
  ;; above version 23 transient-mark-mode is enabled by default
  (when-version% > 23 (transient-mark-mode t)))


;; end of on-transient-autoload.el
