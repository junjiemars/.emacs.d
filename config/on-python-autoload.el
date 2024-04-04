;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(declare-function on-python-init! (v-home%> "config/pythons"))
(declare-function python*-program (v-home%> "config/pythons"))
(declare-function python*-venv! (v-home%> "config/pythons"))
(autoload 'on-python-init! (v-home%> "config/pythons"))

;;; `python' after load
(with-eval-after-load 'python
  (make-thread* #'on-python-init!))

;;; autoload
(autoload 'python*-program (v-home%> "config/pythons")
  "The program of python executable.")
(autoload 'python*-venv-activate! (v-home%> "config/pythons")
  "Activate python\\='s venv." t)

;; end of on-python-autoload.el
