;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-linum-autoload.el
;;;;



;; linum
(if-feature-linum%
  
  (with-eval-after-load 'linum
    (setq% linum-format "%2d " 'linum))

  (if-feature-linum%
    (define-key (current-global-map) (kbd "C-c l") #'linum-mode)))


