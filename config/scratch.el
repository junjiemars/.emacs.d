;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; scratch.el
;;;;


(defvar *scratch-kinds*
  `(("*" . (:msg ,(substitute-command-keys initial-scratch-message)
                 :mod ,(lambda () (lisp-interaction-mode))))
    ("org" . (:msg
              "#+title: Nore Emacs: Org *scratch*
#+author: Nore Emacs

* scratch
	:PROPERTIES:
	:CUSTOM_ID: scratch
	:END:

"
              :mod ,(lambda () (org-mode))))
    ("tex" . (:msg
              "\\documentclass[10pt]{report}
\\makeindex
\\usepackage{amsmath, amsfonts, amssymb, amstext, amscd, amsthm}
\\usepackage{makeidx, graphicx, hyperref, url}
\\usepackage{fontspec}
% \\usepackage[slantfont, boldfont]{xeCJK}
% \\setCJKmainfont{Heiti SC}
\\begin{document}
\\title{Nore Emacs: TeX *scratch*}

$a + b = c$.

\\end{document}
"
              :mod latex-mode
              :pos ,(lambda ()
                      (goto-char (point-min))
                      (forward-line 10)))))
  "Kinds of scratch.")


(defvar *scratch-history* nil
  "Scrach choosing history list.")


(defun scratch (&optional kind)
  "New a *scratch* buffer by KIND or switch to the existing one."
  (interactive
   (list (if current-prefix-arg
             (read-string (format "Choose (%s) "
                                  (mapconcat #'identity
                                             (mapcar #'car *scratch-kinds*)
                                             "|"))
                          (or (car *scratch-history*)
                              (caar *scratch-kinds*))
                          '*scratch-history*)
           (caar *scratch-kinds*))))
  (switch-to-buffer
   (let ((n (format "*%s*" (if (string= "*" kind)
                               "scratch"
                             (concat "scratch-" kind)))))
     (or (get-buffer n)
         (with-current-buffer (get-buffer-create n)
           (when (zerop (buffer-size))
             (let ((k (cdr (assoc** kind *scratch-kinds* :test #'string=))))
               (insert (substring-no-properties (plist-get k :msg)))
               (funcall (plist-get k :mod))
               (when (plist-get k :pos)
                 (funcall (plist-get k :pos)))
               (set-buffer-modified-p nil)))
           (current-buffer))))))



(provide 'scratch)

;;; end of scratch.el
