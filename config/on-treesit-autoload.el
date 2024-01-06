;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-treesit-autoload.el
;;;;

(declare-function on-treesit-init! (v-home%> "config/treesits.el"))

;;; `treesit' after load
(with-eval-after-load 'treesit
  (on-treesit-init!))

;; end of on-treesit-autoload.el
