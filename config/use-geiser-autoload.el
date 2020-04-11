;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser-autoload.el
;;;;


(defmacro-if-feature% geiser)


;; Disable `geiser-mode' for `scheme-mode'
(when-var% geiser-mode-auto-p 'geiser-mode
  (setq% geiser-mode-auto-p nil 'geiser-mode))

(setq% geiser-active-implementations
       (remove** nil 
                 (list (executable-find% "racket")
                       (executable-find% "chicken"))
                 :test #'eq)
       'geiser-mode)


;; end of file
