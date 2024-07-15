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
             (file-in-dirs-p sexp c))
            ((eq op :read)
             (setq c (read-sexp-from-file b)))
            ((eq op :save)
             (save-sexp-to-file (or sexp `(,default-directory)) b))
            ((eq op :path) b)
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
