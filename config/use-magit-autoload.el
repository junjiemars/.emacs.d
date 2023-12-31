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
  (declare (indent 0))
  (if-feature-magit%
      `(progn% ,@body)
    `(comment ,@body)))

;; end of macro


(when-feature-magit%
  (defun on-use-magit-init! ()
    "On using \\=`magit\\=' initialization."
    ;; toggle on `magit-auto-revert-mode'
    (setq% magit-auto-revert-mode t 'magit-autorevert)
    (when-platform% 'windows-nt
      (when% (executable-find% "git")
        ;; On Windows try to open remote git repo via sshx
        ;; will trigger `magit' error: No such file or directory.
        ;; GitHub issue: https://github.com/magit/magit/issues/3345
        (setq% magit-git-executable "git" 'magit)))))


;;; `magit' after load
(when-feature-magit%
  (with-eval-after-load 'magit
    #'on-use-magit-init!))

;;; toggle off `magit-auto-revert-mode'
(when-feature-magit%
  (setq% magit-auto-revert-mode nil 'magit-autorevert))


;; end of use-magit-autoload.el
