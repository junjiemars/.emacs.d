;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-tramp-autoload.el
;;;;

(autoload 'on-tramp-init! (v-home%> "config/tramps"))

;; `tramp' after load
(with-eval-after-load 'tramp
  (make-thread* #'on-tramp-init!))

;;; autoload



;; end of on-tramp-autoload.el
