;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-geiser-autoload.el
;;;;
;; chez
;;; https://scheme.com
;;; http://cisco.github.io/ChezScheme/csug9.5/csug.html
;;;
;; racket : https://racket-lang.org
;;;

(defmacro-if-feature% geiser)


;;; Disable auto `geiser-mode' for `scheme-mode'
(when-var% geiser-mode-auto-p 'geiser-mode
  (setq% geiser-mode-auto-p nil 'geiser-mode))


;;; builtin `chez-mode' and `gambit-mode' better than `geiser'
(when-var% geiser-active-implementations 'geiser-mode
  (setq% geiser-active-implementations
       (remove** nil
                 (list (and (executable-find% "racket") 'racket)
                       (and (executable-find% "scheme") 'chez)
                       (and (executable-find% "gambit") 'gambit)
                       (and (executable-find% "guile") 'guile)
                       (and (executable-find% "chicken" 'chicken)))
                 :test #'eq)
       'geiser-mode)
  (setq% geiser-default-implementation
         (car geiser-active-implementations) 'geiser-mode))


;; eof
