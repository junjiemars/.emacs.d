;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eglot-autoload.el
;;;;

(declare-function on-eglot-init! (v-home%> "config/eglots"))
(autoload 'on-eglot-init! (v-home%> "config/eglots"))

;;; `eglot' after load
(with-eval-after-load 'eglot
  (make-thread* #'on-eglot-init!))

;; end of on-eglot-autoload.el
