;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-module.el
;;;;



(def-feature-supported-p aggressive-indent nil)
(def-feature-supported-p bing-dict nil)
(def-feature-supported-p geiser nil)
(def-feature-supported-p paredit nil)
(def-feature-supported-p rainbow-delimiters nil)


(defmacro feature-allowed-p (feature &rest body)
  "Run BODY when FEATURE supported and allowed."
  (declare (indent 1))
  (let ((supported (intern (format "feature-%s-supported-p" feature))))
    (when (fboundp supported)
      `(,supported
        (package-spec-:allowed-p
          ,@body)))))



(feature-allowed-p aggressive-indent
  ;; enable automatically adjust the identation of code
  ;; https://github.com/Malabarba/aggressive-indent-mode

  (with-eval-after-load 'aggressive-indent

    ;; disable `electric-indent-mode'
    (setq% aggressive-indent-dont-electric-modes t 'aggressive-indent)))
