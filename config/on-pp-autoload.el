;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-pp-autoload.el
;;;;

(autoload 'pp*-xml (v-home%> "config/pps")
  "Pretty print XML." t)
(autoload 'pp*-json (v-home%> "config/pps")
  "Pretty print JSON." t)

(defalias 'pp-html #'pp*-xml)

;; end of on-pp-autoload.el
