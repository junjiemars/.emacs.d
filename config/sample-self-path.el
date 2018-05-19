;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;
;;  self-path.el: locate at `(emacs-home* "private/" "self-path.el")' 
;;;


;; Run order: :env-spec -> :prelogue -> :package-spec -> :epilogue
;; You can point to your Gited Emacs' configuration repo.
;; Default samples `self-*.el' in `(emacs-home* "private/")' directory.
;; :epilogue run in `after-init-hook'


(def-self-path-ref
  :env-spec (emacs-home* "private/self-env-spec.el")
  :prologue (comment (emacs-home* "private/self-prologue.el"))
  :package-spec (comment (emacs-home* "private/self-package-spec.el"))
  :epilogue (comment (emacs-home* "private/self-epilogue.el")))
