;;;; -*- lexical-binding:t -*-
;;;;
;; Navigation
;;;;


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; But be care for some operations will slowdown Emacs




;; smex
(package-supported-p
  (setq-default smex-save-file (v-home! ".smex/" ".smex-items")))



