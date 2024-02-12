;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-mill-autoload.el
;;;;
;; Commentary:
;;;;

(declare-function on-mill-init! (v-home%> "config/mill"))
(autoload 'on-mill-init! (v-home%> "config/mill"))

(make-thread* #'on-mill-init!)

;; end of on-mill-autoload.el
