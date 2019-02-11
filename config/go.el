;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; go.el
;;;;


(require 'thingatpt)



(defun find-position-at-point ()
  "Find symbol at point."
  (list :symbol (let ((symbol (thing-at-point 'symbol)))
                  (when symbol (substring-no-properties symbol)))
        :line (list :begin (line-beginning-position)
                    :end (line-end-position)
                    :text (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))))
      


(provide 'go)

;; end of file
