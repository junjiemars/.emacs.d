;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;

(autoload 'on-dired-init! (v-home%> "config/direds"))
(autoload 'on-dired-aux-init! (v-home%> "config/direds"))
(autoload 'on-arc-mode-init! (v-home%> "config/direds"))

;;; `dired' after load
(with-eval-after-load 'dired
  (make-thread* #'on-dired-init!))

;;; `dired-aux' after load
(with-eval-after-load 'dired-aux
  (make-thread* #'on-dired-aux-init!))

;;; `arc-mode' after load
(with-eval-after-load 'arc-mode
  (make-thread* #'on-arc-mode-init!))

;; end of on-dired-autoload.el
