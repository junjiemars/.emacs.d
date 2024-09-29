;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-python-autoload.el
;;;;

(autoload 'on-python-init! (v-home%> "config/pythons"))
(autoload 'python*-program (v-home%> "config/pythons"))
(autoload 'python*-venv (v-home%> "config/pythons"))
(autoload 'python*-venv-make! (v-home%> "config/pythons") nil t)

;;; `python' after load
(with-eval-after-load 'python
  (make-thread* #'on-python-init!))

;; end of on-python-autoload.el
