;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vcs.el
;;;;

(defalias 'vc*-frontend
  (lexical-let% ((b `(("*" . vc-dir))))
    (lambda (&optional n)
      (cond (n (append! n b t))
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
   (cdr (assoc-string frontend (vc*-frontend)))))



(provide 'vcs)

;; end of vcs.el
