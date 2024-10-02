;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-elisp-autoload.el
;;;;

(autoload 'on-elisp-init! (v-home%> "config/elisps"))
(autoload 'on-ielm-init! (v-home%> "config/elisps"))

;; `elisp-mode'
(when-version% <= 25.0
  (with-eval-after-load 'elisp-mode
    (make-thread* #'on-elisp-init!)))

;; `lisp-mode' after load
(when-version% > 25.0
  (with-eval-after-load 'lisp-mode
    (make-thread* #'on-elisp-init!)))

;; `ielm' after load
(with-eval-after-load 'ielm
  (make-thread* #'on-ielm-init!))

;; end of on-elisp-autoload.el
