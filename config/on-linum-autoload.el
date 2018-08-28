;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-linum-autoload.el
;;;;



;; linum
(feature-linum-supported-p
  
  (with-eval-after-load 'linum
    (setq% linum-format "%2d " linum))

  (feature-linum-supported-p
    (define-key (current-global-map) (kbd "C-c l") #'linum-mode)))


