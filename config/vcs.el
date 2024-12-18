;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vcs.el
;;;;

(defvar vc*-frontend-hook nil
  "Hook called by \\=`vc*-frontend\\='")

(defalias 'vc*-frontend
  (let ((b `(("*" . vc-dir)))
        (i nil))
    (lambda (&optional n)
      (cond (n (append! n b t))
            ((and (not i) vc*-frontend-hook)
             (dolist (x vc*-frontend-hook)
               (append! x b t))
             (setq i t)
             b)
            (t b))))
  "The fontend of VC.")

(defvar *vc-frontend-history* nil
  "The VC frontend choosing history list.")

(defun vc*-dir (&optional frontend)
  "Show the VC status."
  (interactive
   (list (let ((default (or (car *vc-frontend-history*)
                            (caar (vc*-frontend)))))
           (if current-prefix-arg
               (completing-read
                (format "Choose (%s) "
                        (mapconcat #'identity
                                   (mapcar #'car (vc*-frontend))
                                   "|"))
                (vc*-frontend) nil nil
                default
                '*vc-frontend-history* (caar (vc*-frontend)))
             default))))
  (call-interactively
   (cdr (or (assoc-string frontend (vc*-frontend))
            (car (vc*-frontend))))))



(provide 'vcs)

;; end of vcs.el
