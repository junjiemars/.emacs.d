;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; feature.el
;;;;
;; Commentary: builtin features checking.
;;;;

;;; `eglot' builtin since Emacs-29+
(defmacro-if-feature% eglot)

;;; `eww', requires Emacs-24.4+
(defmacro-if-feature% eww)

;;; `project' builtin since Emacs-26+
(defmacro-if-feature% project)

;;; `transient'
(defmacro-if-feature% transient)

;;; `treesit' builtin since Emacs-29+
(defmacro-if-feature% treesit)

;;; `vc'
(defmacro-if-feature% vc)



(provide 'feature)

;; end of feature.el
