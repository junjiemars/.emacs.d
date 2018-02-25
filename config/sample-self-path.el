;;;; -*- lexical-binding:t -*-
;;;
;;  self-path.el: locate at `(emacs-home* "private/" "self-path.el")' 
;;;



;; Run in the order: env-spec -> prelogue -> package-spec -> epilogue
;; You can point to your Gited Emacs' configuration files.
;; There samples `self-*.el' in `(emacs-home* "private/")' directory.

(def-self-path-ref
  :env-spec (emacs-home* "private/self-env-spec.el")
  :prologue (comment (emacs-home* "private/self-prologue.el"))
  :package-spec (comment (emacs-home* "private/self-package-spec.el"))
  :epilogue (comment (emacs-home* "private/self-epilogue.el")))
