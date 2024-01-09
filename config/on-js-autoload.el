;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-js-autoload.el
;;;;


(when-version% > 28.1
  (when-var% js-js-tmpdir 'js
    (with-eval-after-load
        (setq% js-js-tmpdir (v-home! ".js/") 'js))))


;;; end of on-js-autoload.el
