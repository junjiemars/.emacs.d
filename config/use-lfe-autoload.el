;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lfe-autoload.el (Lisp Flavoured Erlang)
;;;;

(autoload 'lfe-mode-hook "lfe-mode" "lfe editing mode." t)
(push! '("\\.lfe$" . lfe-mode) auto-mode-alist)

;; end of use-lfe-autoload.el
