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



(defmacro-if-feature% transient)

(if-feature-transient%
    (with-eval-after-load 'transient
      (let ((dir (v-home! ".transient/")))
        (setq transient-history-file (concat dir "history.el")
              transient-levels-file (concat dir "levels.el")
              transient-values-file (concat dir "values.el")))))


(when-fn% 'magit-status 'magit
  ;; should take up the `vc-create-tag' key binding
  (define-key (current-global-map) (kbd "C-x v M") #'magit-status))



;; end of file
