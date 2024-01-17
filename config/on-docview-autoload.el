;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-docview-autoload.el
;;;;

(declare-function on-doc-view-init! (v-home%> "config/docs"))
(autoload 'on-doc-view-init! (v-home%> "config/docs"))

(with-eval-after-load 'doc-view
  (on-doc-view-init!))

;; end of on-docview-autoload.el
