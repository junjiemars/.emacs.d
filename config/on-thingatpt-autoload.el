;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-thingatpt-autoload.el
;;;;

(autoload 'on-thingatpt-init! (v-home%> "config/thingatpts"))

;; `compile' after load
(with-eval-after-load 'thingatpt
  (make-thread* #'on-thingatpt-init!))

;; end of on-thingatpt-autoload.el
