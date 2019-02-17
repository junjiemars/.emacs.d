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
  "Return a alist of possible root via look up directories of FILE."
  (let ((found nil))
    (catch 'out
      (dir-backtrack
       (file-name-directory file)
       (lambda (d fs)
         (dolist* (f fs)
           (cond ((string= "Makefile" f)
                  (push (list 'makefile (concat d f)) found))
                 ((string= ".git/" f)
                  (throw 'out (push (list 'git (concat d f)) found)))
                 ((string= ".svn/" f)
                  (throw 'out (push (list 'svn (concat d f)) found))))))))))

(defun prefer-project-root (seq &optional prefer)
  "Prefer what as project root."
  (let ((prefer (or prefer '("git" "svn"))))
    (catch 'out
      (dolist* (x prefer)
        (let ((found (alist-get* x seq nil nil #'string=)))
          (when found (throw 'out (car found))))))))


(provide 'go)

;; end of file
