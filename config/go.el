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

(defun find-project-root (file)
  "Return a list of possible root via look up directories of FILE."
  (let ((found nil))
    (catch 'out
      (dir-backtrack (file-name-directory file)
                     (lambda (d fs)
                       (dolist* (f fs)
                         (cond ((string-match "[mM]akefile\\'" f)
                                (push (concat d f) found))
                               ((string-match "/\\.git/\\'\\|/\\.svn/\\'"
                                              (concat d f))
                                (throw 'out
                                       (push (concat d f) found))))))))))

(provide 'go)

;; end of file
