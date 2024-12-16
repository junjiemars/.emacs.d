;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;

(autoload 'use-slime-init! (v-home%> "config/use-slime"))

;; `slime' after load
(with-eval-after-load 'slime
  (make-thread* #'use-slime-init!))

;; end of file
