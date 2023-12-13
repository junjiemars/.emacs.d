;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-js-autoload.el
;;;;

(defun on-js-init! ()
  "On \\=js\\=' initialization."
  (when-version% > 28.1
    (setq% js-js-tmpdir (v-home! ".js/") 'js)))

(eval-after-load 'js #'on-js-init!)


;;; end of on-js-autoload.el
