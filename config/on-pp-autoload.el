;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-pp-autoload.el
;;;;

(autoload 'pp*-xml (v-home%> "config/pps") nil t)
(autoload 'pp*-json (v-home%> "config/pps") nil t)

(defalias 'pp-html #'pp*-xml)

;; end of on-pp-autoload.el
