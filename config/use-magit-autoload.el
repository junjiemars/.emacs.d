;;;; -*- lexical-binding:t -*-
;;
;; use-magit-autoload.el
;;


(defun use-magit! ()
  
  (platform-supported-when 'windows-nt
    
    (when (executable-find% "git" t)
      ;; On Windows try to open remote git repo via sshx
      ;; will trigger `magit' error: No such file or directory.
      ;; GitHub issue: https://github.com/magit/magit/issues/3345
      (setq% magit-git-executable "git" 'magit)))
  
  ;;
  ;; define keys for `magit'
  ;; 
  (when-fn% 'magit-pull 'magit
    (define-key (current-global-map) (kbd "C-c g p") #'magit-pull))
  
  (when-fn% 'magit-push 'magit
    (define-key (current-global-map) (kbd "C-c g P") #'magit-push))
  
  (when-fn% 'magit-log 'magit
    (define-key (current-global-map) (kbd "C-c g l") #'magit-log))

  (when-fn% 'magit-log-buffer-file 'magit
    (define-key (current-global-map) (kbd "C-c g b") #'magit-log-buffer-file))

  (when-fn% 'magit-checkout 'magit
    (define-key (current-global-map) (kbd "C-c g c") #'magit-checkout))

  (when-fn% 'magit-merge 'magit
    (define-key (current-global-map) (kbd "C-c g m") #'magit-merge))
  
  (when-fn% 'magit-fetch 'magit
    (define-key (current-global-map) (kbd "C-c g f") #'magit-fetch)))


(with-eval-after-load 'magit
  (use-magit!))


(when-fn% 'magit-status 'magit
  (define-key (current-global-map) (kbd "C-c g s") #'magit-status))
