;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-help-autoload.el
;;;;

(autoload 'on-help-init! (v-home%> "config/helps"))

;;; `help-mode' after load
(with-eval-after-load 'help-mode
  (make-thread* #'on-help-init!))


;; end of on-help-autoload.el
