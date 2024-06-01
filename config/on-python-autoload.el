;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(declare-function on-python-init! (v-home%> "config/pythons"))
(declare-function python*-program (v-home%> "config/pythons"))
(declare-function python*-venv (v-home%> "config/pythons"))
(declare-function python*-venv-make! (v-home%> "config/pythons"))
(autoload 'on-python-init! (v-home%> "config/pythons"))
(autoload 'python*-program (v-home%> "config/pythons")
  "The program of python executable.")
(autoload 'python*-venv (v-home%> "config/pythons")
  "Python\\='s venv.")
(autoload 'python*-venv-make! (v-home%> "config/pythons")
  "Make python\\='s venv." t)

;;; `python' after load
(with-eval-after-load 'python
  (make-thread* #'on-python-init!))

;; end of on-python-autoload.el
