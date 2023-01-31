;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
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
  "Return alist of possible root via look up directories of FILE."
  (let ((found nil))
    (catch 'out
      (dir-backtrack
       (file-name-directory file)
       (lambda (d fs)
         (dolist* (f fs)
           (cond ((string= "Makefile" f)
                  (push! (list :makefile (concat d f)) found))
                 ((string= ".git/" f)
                  (throw 'out (push! (list :git (concat d f)) found)))
                 ((string= ".svn/" f)
                  (throw 'out (push! (list :svn (concat d f)) found))))))))
    (if (consp found)
        found
      (push! (list :pwd (file-name-directory file)) found))))

(defun prefer-project-root (seq &optional prefer)
  "Prefer what as project root."
  (let ((prefer (or prefer '(:git :svn :makefile :pwd))))
    (catch 'out
      (dolist* (x prefer)
        (let ((found (cdr (assoc** x seq :test #'eq))))
          (when found
            (throw 'out (file-name-directory (car found)))))))))

(defun go-to-definition ()
  "Go to the definition of the symbol at point."
  (interactive)
  (let ((s (find-position-at-point))
        (r (prefer-project-root
            (find-project-root (buffer-file-name (current-buffer))))))
    (compilation-start (format "grep --color=always -I -nH --null -e'%s' -r %s"
                               (plist-get s :symbol)
                               (directory-file-name r)
                               ;; (mapconcat #'identity
                               ;;            (system-cc-include t) " ")
                               )
		                   'grep-mode
                       )))

(provide 'go)

;; end of file
