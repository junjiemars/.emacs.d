;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-dired-autoload.el
;;;;

(declare-function on-dired-init! (v-home%> "config/direds.el"))
(declare-function on-dired-aux-init! (v-home%> "config/direds.el"))
(declare-function browse-file (v-home%> "config/direds.el"))


;;; `dired' after load
(with-eval-after-load 'dired
  (on-dired-init!))

;;; `dired-aux' after load
(with-eval-after-load 'dired-aux
  (on-dired-aux-init!))


;; autoload
(autoload 'dired-get-file-for-visit "dired")
(autoload 'dired-current-directory "dired")
(autoload 'browse-file (v-home%> "config/direds.el"))

(define-key% (current-global-map) (kbd "C-x x B") 'browse-file)

;; end of on-dired-autoload.el
