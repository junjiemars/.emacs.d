;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; projects.el
;;;;

(defalias 'project*-root
  (lexical-let% ((b (emacs-home* ".project/root.el"))
                 (c '()))
    (lambda (&optional op sexp)
      "OP SEXP"
      (cond ((eq op :cache)
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
  "The \\=`project-root\\=' cache.
ROOT must be absolute but can be nested.")

(defun project*-try-root (dir)
  (let ((d (project*-root :cache dir)))
    (when d (list 'vc 'Git d))))

(defun on-project-init! ()
  "On \\=`project\\=' initialization."
  (project*-root :read)
  (push! #'project*-try-root project-find-functions))



(provide 'projects)

;; end of projects.el
