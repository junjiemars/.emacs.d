;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;

(declare-function on-tramp-init! (v-home%> "config/tramps.el"))
(autoload 'on-tramp-init! (v-home%> "config/tramps.el"))

;; `tramp' after load
(with-eval-after-load 'tramp
  (on-tramp-init!))

;;; autoload



;; end of on-tramp-autoload.el
