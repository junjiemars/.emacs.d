;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-elisp-autoload.el
;;;;

(declare-function on-elisp-init! (v-home%> "config/elisps"))
(declare-function on-ielm-init! (v-home%> "config/elisps"))
(autoload 'on-elisp-init! (v-home%> "config/elisps"))
(autoload 'on-ielm-init! (v-home%> "config/elisps"))

;;; `elisp-mode' or `lisp-mode' after load
(with-eval-after-load
    (if-version% <= 25.0 'elisp-mode 'lisp-mode)
  (make-thread*
   (lambda ()
     (inhibit-gc
       (on-elisp-init!)))))

;;; `ielm' after load
(with-eval-after-load 'ielm
  (make-thread* #'on-ielm-init!))

;; end of on-elisp-autoload.el
