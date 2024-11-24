;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ido-autoload.el
;;;;

(autoload 'on-ido-init! (v-home%> "config/idos"))

;;; `ido' after load
(with-eval-after-load 'ido
  (make-thread* #'on-ido-init!))

;; default view file keybindings
(define-key% (current-global-map) "5r" #'view-file-other-frame)
(define-key% (current-global-map) "4r" #'view-file-other-window)
(define-key% (current-global-map) "" #'view-file)


;; end of on-ido-autoload.el
