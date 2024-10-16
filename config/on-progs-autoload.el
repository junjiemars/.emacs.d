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

;; disable suspend-frame
(when-graphic%
  (when-fn% 'suspend-frame 'frame
    (fset 'suspend-frame
          (lambda () (interactive)
            (user-error
             "%s" "A monkey will always behave like a monkey")))))

;; end of on-progs-autoload.el
