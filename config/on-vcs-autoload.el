;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-vcs-autoload.el
;;;;

(declare-function vc*-dir (v-home%> "config/vcs"))
(declare-function vc*-frontend (v-home%> "config/vcs"))
(autoload 'vc*-dir (v-home%> "config/vcs"))
(autoload 'vc*-frontend (v-home%> "config/vcs"))

;; general `vc*-dir'
(define-key% (current-global-map) (kbd "C-x v d") #'vc*-dir)



(provide 'vcs)

;; end of on-vcs-autoload.el
