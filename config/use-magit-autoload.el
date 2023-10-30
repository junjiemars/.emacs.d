;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-magit-autoload.el
;;;;


(defmacro-if-feature% magit)


(defmacro when-feature-magit% (&rest body)
  "When \\=`magit\\=', do BODY."
  (if-feature-magit%
      `(progn% ,@body)))


;;; toggle off `magit-auto-revert-mode'
(when-feature-magit%
 (setq% magit-auto-revert-mode nil 'magit-autorevert))


(when-feature-magit%
 (with-eval-after-load 'magit

   ;; toggle on `magit-auto-revert-mode'
   (setq% magit-auto-revert-mode t 'magit-autorevert)

   (when-platform% 'windows-nt
     (when (executable-find% "git")
       ;; On Windows try to open remote git repo via sshx
       ;; will trigger `magit' error: No such file or directory.
       ;; GitHub issue: https://github.com/magit/magit/issues/3345
       (setq% magit-git-executable "git" 'magit)))))


;; end of use-magit-autoload.el
