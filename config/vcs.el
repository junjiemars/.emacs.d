;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; vcs.el
;;;;

;;; env

(defvar vc*-frontend-hook nil
  "Hook called by \\=`vc*-dir\\='")

(defalias 'vc*-frontend
  (let ((b `(("*" . vc-dir))))
    (lambda (&optional n)
      (cond (n (append! n b delete))
            (t b))))
  "The fontend of VC.")

(defvar *vc-frontend-history* nil
  "The VC frontend choosing history list.")

;; end of env

;;; `vc*-dir'

(defun vc*--dir-prompt ()
  (list (let* ((vcs (apply #'vc*-frontend vc*-frontend-hook))
               (default (or (car *vc-frontend-history*)
                            (caar vcs))))
          (if current-prefix-arg
              (completing-read
               (format "Choose (%s) "
                       (mapconcat #'identity (mapcar #'car vcs) "|"))
               vcs nil nil
               default
               '*vc-frontend-history* (caar vcs))
            default))))

(defun vc*-dir (&optional frontend)
  "Show the VC status."
  (interactive (vc*--dir-prompt))

  (call-interactively
   (cdr (or (assoc-string frontend (vc*-frontend))
            (car (vc*-frontend))))))

;; end of `vcs'

;;; `log-view'

(unless-graphic%
  (with-eval-after-load 'log-view
    (set-face-background 'log-view-message +tui-background-color+)
    (set-face-foreground 'log-view-message +tui-foreground-color+)))

;; end of `log-view'

(provide 'vcs)

;; end of vcs.el
