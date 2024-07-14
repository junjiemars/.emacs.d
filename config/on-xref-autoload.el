;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-xref-autoload.el
;;;;
;; Commentary:
;;;;

(declare-function on-xref-init! (v-home%> "config/xrefs"))
(declare-function on-etags-init! (v-home%> "config/xrefs"))
(declare-function xref*-read-only-dirs (v-home%> "config/xrefs"))
(autoload 'on-xref-init! (v-home%> "config/xrefs"))
(autoload 'on-etags-init! (v-home%> "config/xrefs"))
(autoload 'xref*-read-only-dirs (v-home%> "config/xrefs"))

;;; `xref' after load
(with-eval-after-load 'xref
  (make-thread* #'on-xref-init!))

;;; `etags' after load
(with-eval-after-load 'etags
  (make-thread* #'on-etags-init!))

;;; autoload

;; end of on-xref-autoload.el
