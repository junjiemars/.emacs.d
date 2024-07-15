;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-progs-autoload.el
;;;;

(autoload 'on-progs-init! (v-home%> "config/progs"))

;; delay load modes
(with-eval-after-load 'prog-mode
  (make-thread* #'on-progs-init!))


;; end of on-progs-autoload.el
