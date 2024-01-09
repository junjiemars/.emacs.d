;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-transient-autoload.el
;;;;

(declare-function on-transient-init! (v-home%> "config/transients.el"))
(autoload 'on-transient-init! (v-home%> "config/transients.el"))

(when-feature-transient%
  (with-eval-after-load 'transient
    (on-transient-init!)))


;;; `transient-mark-mode'
(unless-graphic%
  ;; above version 23 transient-mark-mode is enabled by default
  (when-version% > 23 (transient-mark-mode t))
  ;; fix some terminal theme confused with background and foreground.
  (set-face-background 'region "white")
  (set-face-foreground 'region "black"))


;; end of on-transient-autoload.el
