;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit.el
;;;;
;; References:
;; 1. https://developer.mozilla.org/en-US/docs/Web/Accessibility/Understanding_Colors_and_Luminance
;;
;;;;

;; (unless-graphic%

;;   (defun trans-color (color &optional fn frame)
;;     "Trans COLOR by FN in selected FRAME."
;;     (declare (indent 1))
;;     (when (stringp color)
;;       (let ((cs (color-values color frame)))
;;         (when cs
;;           (let ((rc 0) (i 16))
;;             (dolist* (x cs (logand #xffffff rc))
;;               (setq rc (logior
;;                         (ash (if fn (funcall fn x) x) i)
;;                         rc)
;;                     i (- i 8)))))))))

;; (unless-graphic%

;;   (defun toggle-face-background! (face &optional frame)
;;     "Toggle \\=`face-background\\=' to contrasted color."
;;     (let ((c (trans-color (face-background face frame)
;;                (lambda (x) (- #xff x)) frame)))
;;       (and c (set-face-background face (format "#%x" c) frame)))))



(defun use-magit-init! ()
  "On using \\=`magit\\=' initialization."
  ;; toggle on `magit-auto-revert-mode'
  (setq% magit-auto-revert-mode t 'magit-autorevert)
  (when-platform% 'windows-nt
    (when% (executable-find% "git")
      ;; On Windows try to open remote git repo via sshx
      ;; will trigger `magit' error: No such file or directory.
      ;; GitHub issue: https://github.com/magit/magit/issues/3345
      (setq% magit-git-executable "git" 'magit)))
  ;; contrast color for `magit-section-highlight'
  (unless-graphic%
    (set-face-background 'magit-section-highlight +term-background-color+)
    (set-face-foreground 'magit-section-highlight +term-foreground-color+)))



(provide 'use-magit)

;; end of use-magit.el
