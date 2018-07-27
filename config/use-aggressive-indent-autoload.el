;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-aggressive-indent-autoload.el
;;;;



(feature-allowed-p aggressive-indent
  ;; enable automatically adjust the identation of code
  (global-aggressive-indent-mode 1)
  ;; (setq% aggressive-indent-dont-electric-modes t aggressive-indent)
  )
