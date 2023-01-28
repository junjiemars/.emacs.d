;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-module.el
;;;;


(defmacro when-feature-allowed% (if-feature &rest body)
  "Run BODY when FEATURE be supported and be allowed.

IF-FEATURE macro must be defined by `defmacro-if-feature%'."
  (declare (indent 1))
  `(,if-feature
    (package-spec-:allowed-p
      ,@body)))


;; end of on-module.el file
