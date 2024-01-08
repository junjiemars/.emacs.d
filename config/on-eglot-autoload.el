;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;

(declare-function on-eglot-init! (v-home%> "config/eglots.el"))
(autoload 'on-eglot-init! (v-home%> "config/eglots.el"))

;;; `eglot' after load
(with-eval-after-load 'eglot
  (on-eglot-init!))

;; end of on-eglot-autoload.el
