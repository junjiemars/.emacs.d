;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vcs.el
;;;;

(when-feature-vc%
  (defalias 'vc*-frontend
    (lexical-let% ((b `(("*" . vc-dir))))
      (lambda (&optional n)
        (cond (n (append! n b t))
              (t b))))
    "The fontend of VC."))

(when-feature-vc%
  (defvar *vc-frontend-history* nil
    "The VC frontend choosing history list."))

(when-feature-vc%
  (defun vc*-dir (&optional frontend)
    "Show the VC status."
    (interactive
     (list (if current-prefix-arg
               (completing-read
                (format "Choose (%s) "
                        (mapconcat #'identity
                                   (mapcar #'car (vc*-frontend))
                                   "|"))
                (vc*-frontend) nil nil
                (or (car *vc-frontend-history*)
                    (car (vc*-frontend)))
                '*vc-frontend-history* (caar (vc*-frontend)))
             (caar (vc*-frontend)))))
    (call-interactively
     (cdr (assoc-string frontend (vc*-frontend))))))


(when-feature-vc%
  ;; general `vc*-dir'
  (define-key% (current-global-map) (kbd "C-x v d")
               #'vc*-dir))



(provide 'vcs)

;; end of vcs.el
