;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-vcs-autoload.el
;;;;

(autoload 'vc*-dir (v-home%> "config/vcs") nil t)

;; general `vc*-dir'
(define-key% (current-global-map) "vd" #'vc*-dir)


;; end of on-vcs-autoload.el
