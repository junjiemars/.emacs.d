;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(declare-function on-python-init! (v-home%> "config/pythons.el"))
(autoload 'on-python-init! (v-home%> "config/pythons.el"))

;;; `python' after load
(with-eval-after-load 'python
  (on-python-init!))

;;; autoload
(autoload 'python*-program (v-home%> "config/pythons.el"))
(autoload 'python*-activate-venv! (v-home%> "config/pythons.el"))

;; end of on-python-autoload.el
