;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;
;;  self-path.el: locate at `(emacs-home* "private/" "self-path.el")' 
;;;


;; Run order: :env-spec -> :prologue -> :package-spec -> :epilogue
;; You can point to your Gited Emacs' configuration repo.
;; Default samples `sample-self-*.el' in `(emacs-home* "config/")' directory.
;; :epilogue run in `after-init-hook'


(*self-paths* :put :env-spec
              (emacs-home* "self-env-spec.el"))
(*self-paths* :put :prologue
              (comment (emacs-home* "private/self-prologue.el")))
(*self-paths* :put :package-spec
              (comment (emacs-home* "private/self-package-spec.el")))
(*self-paths* :put  :epilogue
              (comment (emacs-home* "private/self-epilogue.el")))

;; eof
