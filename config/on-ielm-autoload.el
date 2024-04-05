;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ielm-autoload.el
;;;;

(declare-function on-ielm-init! (v-home%> "config/lisps"))
(autoload 'on-ielm-init! (v-home%> "config/lisps"))

;;; `ielm' after load
(with-eval-after-load 'ielm
  (make-thread* #'on-ielm-init!))


;; end of on-ielm-autoload.el
