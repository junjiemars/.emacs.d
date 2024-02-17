;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-hippie-autoload.el
;;;;

(declare-function on-hippie-init! (v-home%> "config/hippies"))
(autoload 'on-hippie-init! (v-home%> "config/hippies"))

(with-eval-after-load 'hippie-exp
  (make-thread* #'on-hippie-init!))


;; Key binding to use "hippie expand" for text autocompletion
;; See also: http://www.emacswiki.org/emacs/HippieExpand
(define-key% (current-global-map) (kbd "M-/") #'hippie-expand)


;; end of on-hippie-autoload.el
