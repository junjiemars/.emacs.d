;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mixal-autoload.el
;;;;

(autoload 'on-mixal-mode-init! (v-home%> "config/mixal"))

;;; after-load
(with-eval-after-load 'mixal-mode
  (make-thread* #'on-mixal-mode-init!))

;; end of on-mixal-autoload.el
