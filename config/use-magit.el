;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit.el
;;;;

(defun use-magit-init! ()
  "On using \\=`magit\\=' initialization."
  ;; toggle on `magit-auto-revert-mode'
  (setq% magit-auto-revert-mode t 'magit-autorevert)
  (when-platform% 'windows-nt
    (when% (executable-find% "git")
      ;; On Windows try to open remote git repo via sshx
      ;; will trigger `magit' error: No such file or directory.
      ;; GitHub issue: https://github.com/magit/magit/issues/3345
      (setq% magit-git-executable "git" 'magit))))



(provide 'use-magit)

;; end of use-magit.el
