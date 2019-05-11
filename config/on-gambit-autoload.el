;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-gambit-autoload.el
;;;;


(autoload 'gambit-mode "gambit" "Scheme minor mode for Gambit." t)
(autoload 'run-gambit "gambit" "Run Gambit REPL.." t)

(defmacro-if-feature% geiser)

;; Disable `geiser-mode' for `schme-mode'
(if-feature-geiser%
  (when-var% geiser-mode-auto-p 'geiser-mode
    (setq% geiser-mode-auto-p nil 'geiser-mode)))

;; end of file
