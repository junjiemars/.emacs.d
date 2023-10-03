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


(when-feature-magit%
 (when-platform% 'windows-nt
   (with-eval-after-load 'magit

     (when (executable-find% "git")
       ;; On Windows try to open remote git repo via sshx
       ;; will trigger `magit' error: No such file or directory.
       ;; GitHub issue: https://github.com/magit/magit/issues/3345
       (setq% magit-git-executable "git" 'magit)))))


;; end of use-magit-autoload.el
