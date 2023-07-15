;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-ido-autoload.el
;;;;


;; default view file keybindings
(define-key% (current-global-map) (kbd "C-x 5 r") #'view-file-other-frame)
(define-key% (current-global-map) (kbd "C-x 4 r") #'view-file-other-window)
(define-key% (current-global-map) (kbd "C-x C-r") #'view-file)





(with-eval-after-load 'ido

  ;; `ido' override default view file keybindings
  (define-key% (current-global-map)
    (kbd "C-x 5 r") #'ido-find-file-read-only-other-frame)
  (define-key% (current-global-map)
    (kbd "C-x 4 r") #'ido-find-file-read-only-other-window)
  (define-key% (current-global-map)
    (kbd "C-x C-r") #'ido-find-file-read-only)

  (setq ido-enable-flex-matching t))


 ;; end of on-ido-autoload.el
