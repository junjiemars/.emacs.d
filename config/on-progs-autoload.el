;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-progs-autoload.el
;;;;

(declare-function on-progs-init! (v-home%> "config/progs.el"))

;; delay load modes
(make-thread* #'on-progs-init!)


;; end of on-progs-autoload.el
