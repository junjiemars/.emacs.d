;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-js-autoload.el
;;;;

(defun on-js-init! ()
  "On \\=js\\=' initialization."
  (when-var% js-js-tmpdir 'js
    (setq% js-js-tmpdir (v-home! ".js/") 'js)))

(eval-after-load 'js #'on-js-init!)


;;; end of on-js-autoload.el
