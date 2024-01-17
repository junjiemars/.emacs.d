;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-eshell-autoload.el
;;;;

(declare-function on-eshell-init! (v-home%> "config/eshells"))
(autoload 'on-eshell-init! (v-home%> "config/eshells"))

;;; `eshell' after load
(with-eval-after-load 'eshell
  (on-eshell-init!))


;; end of on-eshell-autoload.el
