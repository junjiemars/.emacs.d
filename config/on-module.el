;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-module.el
;;;;


(defmacro-if-feature% aggressive-indent)
(defmacro-if-feature% paredit)
(defmacro-if-feature% rainbow-delimiters)


(defmacro when-feature-allowed% (feature &rest body)
  "Run BODY when FEATURE be supported and be allowed."
  (declare (indent 1))
  (let ((supported (intern (format "if-feature-%s%%" feature))))
    (with-no-warnings
      `(,supported
        (package-spec-:allowed-p
          ,@body)))))


;; end of on-module.el file
