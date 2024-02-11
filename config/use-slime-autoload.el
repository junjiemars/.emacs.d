;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;

(declare-function use-slime-init! (v-home%> "config/use-slime"))
(autoload 'use-slime-init! (v-home%> "config/use-slime"))

;; `slime' after load
(with-eval-after-load 'slime
  (use-slime-init!))

;; end of file
