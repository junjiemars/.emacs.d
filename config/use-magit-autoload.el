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

;; end of macro


(when-feature-magit%
 (defun on-magit-init! ()
   "On \\=`magit\\=' initialization."
   ;; toggle on `magit-auto-revert-mode'
   (setq% magit-auto-revert-mode t 'magit-autorevert)
   (when-platform% 'windows-nt
     (when (executable-find% "git")
       ;; On Windows try to open remote git repo via sshx
       ;; will trigger `magit' error: No such file or directory.
       ;; GitHub issue: https://github.com/magit/magit/issues/3345
       (setq% magit-git-executable "git" 'magit)))))

;;; toggle off `magit-auto-revert-mode'
(when-feature-magit%
 (setq% magit-auto-revert-mode nil 'magit-autorevert))


;;; `magit' after load
(when-feature-magit%
 (with-eval-after-load 'magit
   (on-magit-init!)))


;; end of use-magit-autoload.el
