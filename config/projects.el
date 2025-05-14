;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; projects.el
;;;;

;;; requires

;; end of requires

(defalias 'project*-root-dirs
  (let ((f (v-home% ".exec/project-root-dirs.el"))
        (b '()))
    (lambda (&optional op dir)
      (cond ((and op (eq op :find)) (file-in-dirs-p dir b))
            ((and op (eq op :read)) (setq b (read-file* f t)))
            ((and op (eq op :save)) (write-file* (or dir b) f))
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
