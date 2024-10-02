;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-cc-autoload.el
;;;;
;; Commentary: `cc' autoload.
;;;;

(autoload 'on-cc-mode-init! (v-home%> "config/cc"))
(autoload 'on-man-init! (v-home%> "config/cc"))
(autoload 'cc*-cc (v-home%> "config/cc"))
(autoload 'cc*-system-include (v-home%> "config/cc"))
(autoload 'cc*-make-tags (v-home%> "config/cc"))


;; default `c-mode-hook'
;; involving useless `macrostep-c-mode-hook'.
;; (setq% c-mode-hook nil 'cc-mode)

;;; `cc-mode' after load
(with-eval-after-load 'cc-mode
  (make-thread* #'on-cc-mode-init!))


;;; `man' after load
(with-eval-after-load 'man
  (make-thread* #'on-man-init!))


;; end of on-cc-autoload.el
