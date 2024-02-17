;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;

(declare-function on-isearch-init! (v-home%> "config/isearchs"))
(autoload 'on-isearch-init! (v-home%> "config/isearchs"))

;;; after load
(if-version%
    < 25
    (with-eval-after-load 'isearch
      (make-thread* #'on-isearch-init!))
  (make-thread* #'on-isearch-init!))

;;; autoload


;; end of on-isearch-autoload.el
