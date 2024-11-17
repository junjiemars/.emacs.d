;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-treesit-autoload.el
;;;;

(autoload 'on-treesit-init! (v-home%> "config/treesits"))
(autoload 'toggle-treesit! (v-home%> "config/treesits") nil t)

;;; `treesit' after load
(with-eval-after-load 'treesit
  (make-thread* #'on-treesit-init!))

;; end of on-treesit-autoload.el
