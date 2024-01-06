;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mixal-autoload.el
;;;;

(declare-function on-mixal-mode-init! (v-home%> "config/mixal.el"))

;;; after-load
(with-eval-after-load 'mixal-mode
  (on-mixal-mode-init!))


;; end of on-mixal-autoload.el
