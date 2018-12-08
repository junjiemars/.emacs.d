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
  (define-key% (current-global-map) (kbd "C-c g p") (if-fn% 'magit-pull-branch 'magit
                                                            #'magit-pull-branch
                                                      #'magit-pull))
  
  (define-key% (current-global-map) (kbd "C-c g P") (if-fn% 'magit-push-other 'magit
                                                            'magit-push-other
                                                      #'magit-push))
  
  (define-key% (current-global-map) (kbd "C-c g l") (if-fn% #'magit-log-other 'magit
                                                            #'magit-log-other
                                                      #'magit-log))

  (define-key% (current-global-map) (kbd "C-c g b") #'magit-log-buffer-file)

  (define-key% (current-global-map) (kbd "C-c g c") #'magit-checkout)

  (define-key% (current-global-map) (kbd "C-c g m") (if-fn% #'magit-merge-plain 'magit
                                                            #'magit-merge-plain
                                                      #'magit-merge))
  
  (define-key% (current-global-map) (kbd "C-c g f") (if-fn% #'magit-fetch-other 'magit
                                                            #'magit-fetch-other
                                                      #'magit-fetch)))


(with-eval-after-load 'magit
  (use-magit!))


(when-fn% 'magit-status 'magit
  (define-key (current-global-map) (kbd "C-c g s") #'magit-status))
