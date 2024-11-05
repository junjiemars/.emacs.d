;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; projects.el
;;;;

(defalias 'project*-root-dirs
  (lexical-let% ((f (v-home% ".exec/project-root-dirs.el"))
                 (b '()))
    (lambda (&optional op dir)
      (cond ((eq op :find) (file-in-dirs-p dir b))
            ((eq op :read) (setq b (read-sexp-from-file f)))
            ((eq op :save) (save-sexp-to-file (or dir b) f))
            ((eq op :file) f)
            (t b))))
  "The root dirs for \\=`project\\='.")

(defun project*-try-root (dir)
  (let ((d (project*-root-dirs :find dir)))
    (when d (list 'vc 'Git d))))

(defun on-project-init! ()
  "On \\=`project\\=' initialization."
  (project*-root-dirs :read)
  (push! #'project*-try-root project-find-functions))



(provide 'projects)

;; end of projects.el
