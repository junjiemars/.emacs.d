;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lisp-autoload.el
;;;;

(autoload 'use-emacs-lisp-init! (v-home%> "config/use-lisp"))
(autoload 'use-lisp-init! (v-home%> "config/use-lisp"))
(autoload 'use-scheme-init! (v-home%> "config/use-lisp"))

;;; `elisp-mode' after load
(with-eval-after-load 'elisp-mode
  (make-thread* #'use-emacs-lisp-init!))

;;; `lisp-mode' after load
(with-eval-after-load 'lisp-mode
  (make-thread* #'use-lisp-init!))

;;; `scheme' after load
(with-eval-after-load 'scheme
  (make-thread* #'use-scheme-init!))


;; end of use-lisp-autoload.el
