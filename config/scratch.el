;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; scratch.el
;;;;


(defvar *scratch-recipe*
  `(("*" . (:msg ,(substitute-command-keys initial-scratch-message)
                 :mod ,(lambda () (lisp-interaction-mode))))
    ("org" . (:msg
              "#+title: Scratch Org
#+author: Nore Emacs

* scratch
  :PROPERTIES:
  :CUSTOM_ID: scratch
  :END:

# This buffer is for /Org/ that is not saved.


"
              :mod ,(lambda () (org-mode))
              :pos ,(lambda ()
                      (goto-char (point-min))
                      (forward-line 8))))
    ("tex" . (:msg
              "\\documentclass{article}
\\title{Scratch Tex}
\\author{Nore Emacs}
\\usepackage{amsmath}
% \\usepackage{amssymb}
% \\usepackage{xeCJK}
% \\setCJKmainfont{SimSong}
% \\setmainfont{Times New Roman}
\\begin{document}
\\maketitle

% This buffer is for \\TeX{} that is not saved.

\\end{document}
"
              :mod latex-mode
              :pos ,(lambda ()
                      (goto-char (point-min))
                      (forward-line 5)))))
  "The recipes of scratch.")


(defvar *scratch-recipe-history* nil
  "The scratch recipe choosing history list.")


(defun scratch (&optional recipe)
  "New a *scratch* buffer by RECIPE or switch to the existing one."
  (interactive
   (list (if current-prefix-arg
             (completing-read
              (format "Choose (%s) "
                      (mapconcat #'identity
                                 (mapcar #'car *scratch-recipe*)
                                 "|"))
              (mapcar #'car *scratch-recipe*)
              nil nil (car *scratch-recipe-history*)
              '*scratch-recipe-history* (caar *scratch-recipe*))
           (caar *scratch-recipe*))))
  (switch-to-buffer
   (let ((n (format "*%s*" (if (string= "*" recipe)
                               "scratch"
                             (concat "scratch-" recipe)))))
     (or (get-buffer n)
         (with-current-buffer (get-buffer-create n)
           (when (zerop (buffer-size))
             (let ((k (cdr (assoc** recipe *scratch-recipe* :test #'string=))))
               (insert (substring-no-properties (plist-get k :msg)))
               (funcall (plist-get k :mod))
               (when (plist-get k :pos)
                 (funcall (plist-get k :pos)))
               (set-buffer-modified-p nil)))
           (current-buffer))))))



(provide 'scratch)

;; end of scratch.el
