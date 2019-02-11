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
    (let ((symbol (thing-at-point 'symbol)))
      (when symbol (substring-no-properties symbol)))))


(provide 'go)

;; end of file
