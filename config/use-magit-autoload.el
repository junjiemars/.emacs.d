;;;; -*- lexical-binding:t -*-
;;
;; use-magit-autoload.el
;;

(defmacro-if-feature% transient)


(with-eval-after-load 'magit

  (when-platform% 'windows-nt

    (when (executable-find% "git")
      ;; On Windows try to open remote git repo via sshx
      ;; will trigger `magit' error: No such file or directory.
      ;; GitHub issue: https://github.com/magit/magit/issues/3345
      (setq% magit-git-executable "git" 'magit)))

  ;; log root: `vc-print-root-log'
  (define-key% (current-global-map) (kbd "C-x v L") #'magit-log-current)

  ;; log current buffer: `vc-print-log'
  (define-key% (current-global-map) (kbd "C-x v l") #'magit-log-buffer-file)

  ;; history region: `vc-region-history'
  (define-key% (current-global-map) (kbd "C-x v h") #'magit-log-buffer-file)

  ;; compare root diff: `vc-root-diff'
  (define-key% (current-global-map) (kbd "C-x v D") #'vc-root-diff)

  ;; compare buffer diff: `vc-diff'
  (define-key% (current-global-map) (kbd "C-x v =") #'vc-diff))



(if-feature-transient%
    (with-eval-after-load 'transient
      (let ((dir (v-home! ".transient/")))
        (setq transient-history-file (concat dir "history.el")
              transient-levels-file (concat dir "levels.el")
              transient-values-file (concat dir "values.el")))))


(when-fn% 'magit-status 'magit
  ;; should take up the `vc-dir' key binding
  (define-key (current-global-map) (kbd "C-x v d") #'magit-status))



;; end of file
