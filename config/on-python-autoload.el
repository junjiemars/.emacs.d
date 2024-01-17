;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(declare-function on-python-init! (v-home%> "config/pythons"))
(autoload 'on-python-init! (v-home%> "config/pythons"))

;;; `python' after load
(with-eval-after-load 'python
  (on-python-init!))

;;; autoload
(autoload 'python*-program (v-home%> "config/pythons")
  "The program of python executable.")
(autoload 'python*-activate-venv! (v-home%> "config/pythons")
  "Activate python\\'s virtualenv." t)

;; end of on-python-autoload.el
