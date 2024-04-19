;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lisp-autoload.el
;;;;

(declare-function use-emacs-lisp-init! (v-home%> "config/use-lisp"))
(declare-function use-lisp-init! (v-home%> "config/use-lisp"))
(declare-function use-scheme-init! (v-home%> "config/use-lisp"))
(autoload 'use-emacs-lisp-init! (v-home%> "config/use-lisp"))
(autoload 'use-lisp-init! (v-home%> "config/use-lisp"))
(autoload 'use-scheme-init! (v-home%> "config/use-lisp"))

;;; `elisp-mode' after load
(with-eval-after-load 'elisp-mode
  (use-emacs-lisp-init!))

;;; `lisp-mode' after load
(with-eval-after-load 'lisp-mode
  (use-lisp-init!))

;;; `scheme' after load
(with-eval-after-load 'scheme
  (use-scheme-init!))


;; end of use-lisp-autoload.el
