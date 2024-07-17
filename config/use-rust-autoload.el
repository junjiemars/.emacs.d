;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-rust-autoload.el
;;;;

(autoload 'use-rust-init! (v-home%> "config/use-rust"))

;; `rust' after load
(with-eval-after-load 'rust-mode
  (make-thread* #'use-rust-init!))

;; end of use-rust-autoload.el
