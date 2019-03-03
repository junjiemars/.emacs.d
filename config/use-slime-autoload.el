;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime-autoload.el
;;;;


(with-eval-after-load 'slime

  (when-fn% 'set-slime-lisp-implementations! 'use-slime
    (require 'use-slime)
    (set-slime-lisp-implementations!))

  (when-fn% 'slime-setup 'slime
    (slime-setup '(slime-fancy slime-asdf)))

  (when-fn% 'slime-selector 'slime
    (define-key (current-global-map) (kbd "C-c s s") #'slime-selector)))


;; end of file
