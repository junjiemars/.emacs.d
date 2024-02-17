;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-term-autoload.el
;;;;

(declare-function term*-unify-shell-prompt (v-home%> "config/terms"))
(declare-function on-term-init! (v-home%> "config/terms"))
(autoload 'term*-unify-shell-prompt (v-home%> "config/terms")
  "Unify shell prompt." t)
(autoload 'on-term-init! (v-home%> "config/terms"))

(with-eval-after-load 'term
  (make-thread* #'on-term-init!))

;; end of on-term-autoload.el
