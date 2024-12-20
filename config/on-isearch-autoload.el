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

(define-global-key% "" #'isearch*-forward)
(define-global-key% "" #'isearch*-backward)
(define-global-key% (kbd "M-s .") #'isearch*-forward-symbol)

;; end of on-isearch-autoload.el
