;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; projects.el
;;;;

(defalias 'project*-root-dirs
  (lexical-let% ((b (v-home% ".exec/project-root-dirs.el"))
                 (c '()))
    (lambda (&optional op sexp)
      (cond ((eq op :find)
             (if sexp
                 (catch 'br
                   (dolist* (s1 c)
                     (when (string= s1 sexp)
                       (throw 'br s1))))
               c))
            ((eq op :read)
             (setq c (read-sexp-from-file b)))
            ((eq op :save)
             (when sexp (save-sexp-to-file sexp b)))
            (t c))))
  "The root dirs for `project'.")

(defun project*-try-root (dir)
  (let ((d (project*-root-dirs :find dir)))
    (when d (list 'vc 'Git d))))

(defun on-project-init! ()
  "On \\=`project\\=' initialization."
  (project*-root-dirs :read)
  (push! #'project*-try-root project-find-functions))



(provide 'projects)

;; end of projects.el
