;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-xref-autoload.el
;;;;
;; Commentary:
;;;;

(declare-function on-xref-init! (v-home%> "config/xrefs.el"))
(declare-function on-etags-init! (v-home%> "config/xrefs.el"))

;;; `xref' after load
(with-eval-after-load 'xref
  (on-xref-init!))

;;; `etags' after load
(with-eval-after-load 'etags
  (on-etags-init!))



;; end of on-xref-autoload.el
