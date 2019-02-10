;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-isearch-autoload.el
;;;;

;; regexp search and replace should be first:
;; interactive search key bindings.
;; by default, [C-s] runs `isearch-forward', so this swaps the bindings.
;; [C-M-s] or [C-u C-s] do `isearch-forward'
(define-key (current-global-map) (kbd "C-s") #'isearch-forward-regexp)
(define-key (current-global-map) (kbd "C-r") #'isearch-backward-regexp)


;; end of file
