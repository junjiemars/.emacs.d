;;;; -*- lexical-binding:t -*-
;;
;; use-magit-autoload.el
;;


(with-eval-after-load 'magit

  (when-platform% 'windows-nt
    
    (when (executable-find% "git" t)
      ;; On Windows try to open remote git repo via sshx
      ;; will trigger `magit' error: No such file or directory.
      ;; GitHub issue: https://github.com/magit/magit/issues/3345
      (setq% magit-git-executable "git" 'magit))))


(when-fn% 'magit-status 'magit
  ;; should take up the `vc-create-tag' key binding
  (define-key (current-global-map) (kbd "C-x v s")
    #'magit-status))


;; end of file
