;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ido-autoload.el
;;;;

(declare-function on-ido-init! (v-home%> "config/idos.el"))
(autoload 'on-ido-init! (v-home%> "config/idos.el"))

;;; `ido' after load
(with-eval-after-load 'ido
  (on-ido-init!))

;; default view file keybindings
(define-key% (current-global-map) (kbd "C-x 5 r")
             #'view-file-other-frame)
(define-key% (current-global-map) (kbd "C-x 4 r")
             #'view-file-other-window)
(define-key% (current-global-map) (kbd "C-x C-r")
             #'view-file)


;; end of on-ido-autoload.el
