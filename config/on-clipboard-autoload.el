;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-clipboard-autoload.el
;;;;

(autoload 'clipboard-x-kill (v-home%> "config/clipboard"))
(autoload 'clipboard-x-yank (v-home%> "config/clipboard"))

;;; enable `select'
(if-version%
    <= 24.1
    (setq% select-enable-clipboard t)
  (setq% x-select-enable-clipboard t))
(if-version%
    <= 25.1
    (setq% select-enable-primary t 'select)
  (setq% x-select-enable-primary t 'select))

;; kill/yank
(unless-platform% 'windows-nt
  (setq interprogram-cut-function #'clipboard-x-kill
        interprogram-paste-function #'clipboard-x-yank))

 ;; end of on-clipboard-autoload.el
