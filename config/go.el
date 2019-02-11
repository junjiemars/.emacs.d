;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; go.el
;;;;


(require 'thingatpt)



(defun find-symbol-at-point ()
  "Find symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning)
                                      (region-end))
    (version-supported-if <= 24.4
                          (thing-at-point 'symbol t)
      (thing-at-point 'symbol))))


(provide 'go)

;; end of file
