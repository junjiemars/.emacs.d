;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-compile-autoload.el
;;;;

(autoload 'on-compile-init! (v-home%> "config/compiles"))
(autoload 'on-grep-init! (v-home%> "config/compiles"))
(autoload 'on-make-mode-init! (v-home%> "config/compiles"))

;; `compile' after load
(with-eval-after-load 'compile
  (make-thread* #'on-compile-init!))

;;; `grep' after load
(with-eval-after-load 'grep
  (make-thread* #'on-grep-init!))

;;; `make-mode' after load
(with-eval-after-load 'make-mode
  (make-thread* #'on-make-mode-init!))

;; `compile' global key
(define-key% (current-global-map) (kbd "C-x p c") #'compile)

;; end of on-compile-autoload.el
