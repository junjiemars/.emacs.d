;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-progs-autoload.el
;;;;

(declare-function on-progs-init! (v-home%> "config/progs"))
(autoload 'on-progs-init! (v-home%> "config/progs"))

;; delay load modes
(with-eval-after-load 'prog-mode
  (make-thread*
   (lambda ()
     (inhibit-gc
       (on-progs-init!)))))


;; end of on-progs-autoload.el
