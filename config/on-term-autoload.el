;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-term-autoload.el
;;;;

(declare-function on-term-init! (v-home%> "config/terms"))
(declare-function term*-unify-shell-prompt (v-home%> "config/terms"))
(declare-function on-eshell-init! (v-home%> "config/eshells"))
(declare-function on-ielm-init! (v-home%> "config/elisps"))
(autoload 'on-term-init! (v-home%> "config/terms"))
(autoload 'on-eshell-init! (v-home%> "config/eshells"))
(autoload 'on-ielm-init! (v-home%> "config/elisps"))

;;; `term'
(with-eval-after-load 'term
  (make-thread* #'on-term-init!))

;;; `eshell'
(with-eval-after-load 'eshell
  (make-thread* #'on-eshell-init!))

;;; `ielm' after load
(with-eval-after-load 'ielm
  (make-thread* #'on-ielm-init!))

;;; autoload
(autoload 'term*-unify-shell-prompt (v-home%> "config/terms")
  "Unify shell prompt." t)

;; end of on-term-autoload.el
