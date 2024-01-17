;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-compile-autoload.el
;;;;

(declare-function on-compile-init! (v-home%> "config/compiles"))
(declare-function on-grep-init! (v-home%> "config/compiles"))
(declare-function on-make-mode-init! (v-home%> "config/compiles"))
(autoload 'on-compile-init! (v-home%> "config/compiles"))
(autoload 'on-grep-init! (v-home%> "config/compiles"))
(autoload 'on-make-mode-init! (v-home%> "config/compiles"))


;; `compile' after load
(with-eval-after-load 'compile
  (on-compile-init!))

;;; `grep' after load
(with-eval-after-load 'grep
  (on-grep-init!))

;;; `make-mode' after load
(with-eval-after-load 'make-mode
  (on-make-mode-init!))

;;; autoload

;; `compile' global key
(define-key% (current-global-map) (kbd "C-x p c") #'compile)




;; end of on-compile-autoload.el
