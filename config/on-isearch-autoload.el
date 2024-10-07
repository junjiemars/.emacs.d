;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;


(autoload 'isearch*-forward (v-home%> "config/isearchs"))
(autoload 'isearch*-backward (v-home%> "config/isearchs"))
(autoload 'isearch*-forward-symbol (v-home%> "config/isearchs"))

(define-key% (current-global-map) (kbd% "C-s") #'isearch*-forward)
(define-key% (current-global-map) (kbd% "C-r") #'isearch*-backward)
(define-key% (current-global-map) (kbd% "M-s .") #'isearch*-forward-symbol)

;; end of on-isearch-autoload.el
