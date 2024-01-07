;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(declare-function on-python-init! (v-home%> "config/pythons.el"))

;;; `python' after load
(with-eval-after-load 'python
  (on-python-init!))


;; end of on-python-autoload.el
