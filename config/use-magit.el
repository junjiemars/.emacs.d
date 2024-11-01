;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit.el
;;;;

(unless-graphic%

  (defun color-contrast (color &optional frame)
    "Contrast COLOR to integer in selected FRAME."
    (when (stringp color)
      (let ((cs (color-values color frame)))
        (when cs
          (let ((rc 0) (i 16))
            (dolist* (x cs (logand #xffffff rc))
              (setq rc (logior (ash (+ #x13 (lognot x)) i) rc)
                    i (- i 8)))))))))

(unless-graphic%

  (defun toggle-face-background! (face &optional frame)
    "Toggle \\=`face-background\\= to contrasted color."
    (let ((c (color-contrast (face-background face frame))))
      (when c
        (set-face-background face (format "#%x" c) frame)))))



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
    (toggle-face-background! 'magit-section-highlight)))



(provide 'use-magit)

;; end of use-magit.el
