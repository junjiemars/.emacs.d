;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
;;;;

(declare-function on-elisp-init! (v-home%> "config/lisps.el"))
(declare-function on-ielm-init! (v-home%> "config/lisps.el"))
(autoload 'on-elisp-init! (v-home%> "config/lisps.el"))
(autoload 'on-ielm-init! (v-home%> "config/lisps.el"))

;;; `elisp-mode' or `lisp-mode' after load
(with-eval-after-load
    (if-version% <= 25.0 'elisp-mode 'lisp-mode)
  (on-elisp-init!))

;;; `ielm' after load
(with-eval-after-load 'ielm
  (on-ielm-init!))

;;; autoload

;; end of on-lisp-autoload.el
