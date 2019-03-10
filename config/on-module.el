;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-module.el
;;;;



(_defmacro-feature-supported-p aggressive-indent nil)
(_defmacro-feature-supported-p bing-dict nil)
(_defmacro-feature-supported-p paredit nil)
(_defmacro-feature-supported-p rainbow-delimiters nil)


(defmacro feature-allowed-p (feature &rest body)
  "Run BODY when FEATURE supported and allowed."
  (declare (indent 1))
  (let ((supported (intern (format "feature-%s-supported-p" feature))))
    (when (fboundp supported)
      `(,supported
        (package-spec-:allowed-p
          ,@body)))))


;; end of on-module.el file
