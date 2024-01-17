;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-term-autoload.el
;;;;

(declare-function term*-unify-shell-prompt (v-home%> "config/terms"))
(when-platform% 'windows-nt
  (declare-function on-term-init! (v-home%> "config/terms"))
  (autoload 'on-term-init! (v-home%> "config/terms")))

(when-platform% 'windows-nt
  (with-eval-after-load 'term
  	(on-term-init!)))

;;; autoload
(autoload 'term*-unify-shell-prompt (v-home%> "config/terms")
  "Unify shell prompt." t)

;; end of on-term-autoload.el
