;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; projects.el
;;;;

;;; requires

(eval-when-compile
  (require 'ed (v-home%> "config/ed")))

;; end of requires

(defalias 'project*-root-dirs
  (let ((f (v-home% ".exec/project-root-dirs.el"))
        (b '()))
    (lambda (&optional op dir)
      (cond ((and op (eq op :find)) (file-in-dirs-p dir b))
            ((and op (eq op :read)) (setq b (read-sexp-from-file f)))
            ((and op (eq op :save)) (save-sexp-to-file (or dir b) f))
            ((and op (eq op :file)) f)
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
