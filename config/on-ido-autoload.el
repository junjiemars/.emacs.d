;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ido-autoload.el
;;;;


(defalias 'on-view-key!
  (lexical-let% ((b nil))
    (lambda ()
      (unless b
        (define-key% (current-global-map) (kbd "C-x 5 r")
                     #'view-file-other-frame)
        (define-key% (current-global-map) (kbd "C-x 4 r")
                     #'view-file-other-window)
        (define-key% (current-global-map) (kbd "C-x C-r")
                     #'view-file)
        (setq b t)))))



(defun on-ido-init! ()
  "On \\=`ido\\=' intialization."
  (when-version% > 28
    (make-thread*
     (require 'dired-x nil t)))
  (define-key% (current-global-map) (kbd "C-x 5 r")
               #'ido-find-file-read-only-other-frame)
  (define-key% (current-global-map) (kbd "C-x 4 r")
               #'ido-find-file-read-only-other-window)
  (define-key% (current-global-map) (kbd "C-x C-r")
               #'ido-find-file-read-only)
  (setq% ido-enable-flex-matching t 'ido))


;;; `ido' after load
(with-eval-after-load 'ido
  #'on-ido-init!)

;; default view file keybindings
(on-view-key!)

;; end of on-ido-autoload.el
