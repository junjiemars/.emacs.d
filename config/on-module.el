;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-module.el
;;;;



(def-feature-supported-p aggressive-indent nil
  "If `aggressive-indent' feature supported then do BODY.")


(def-feature-supported-p paredit nil
  "If `paredit' feature supported then do BODY.")


(def-feature-supported-p bing-dict nil)


(defmacro feature-allowed-p (feature &rest body)
  "Run BODY when FEATURE supported and allowed."
  (declare (indent 1))
  (let ((supported (intern (format "feature-%s-supported-p" feature))))
    (when (fboundp supported)
      `(,supported
	(package-spec-:allowed-p
	  ,@body)))))
