;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-term-autoload.el
;;;;

(autoload 'on-eshell-init! (v-home%> "config/eshells"))
(autoload 'on-ielm-init! (v-home%> "config/elisps"))
(autoload 'on-term-init! (v-home%> "config/terms"))
(autoload 'term*-unify-shell-prompt (v-home%> "config/terms") nil t)

;;; `term'
(with-eval-after-load 'term
  (make-thread* #'on-term-init!))

;;; `eshell'
(with-eval-after-load 'eshell
  (make-thread* #'on-eshell-init!))

;;; `ielm' after load
(with-eval-after-load 'ielm
  (make-thread* #'on-ielm-init!))

;; fix some terminal theme confused with background and foreground.
(unless-graphic%
  (set-face-background 'region "yellow")
  (set-face-foreground 'region "black"))

;; end of on-term-autoload.el
