;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-elisp-autoload.el
;;;;

(autoload 'on-elisp-init! (v-home%> "config/elisps"))

;; `elisp-mode' and `lisp-mode' after load
(if-version%
    <= 25.0
    (with-eval-after-load 'elisp-mode
      (make-thread* #'on-elisp-init!))
  (with-eval-after-load 'lisp-mode
    (make-thread* #'on-elisp-init!)))

;; end of on-elisp-autoload.el
