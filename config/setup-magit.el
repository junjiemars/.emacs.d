;;
;; Magit
;;

;; shortcuts
(safe-do-when magit-status
  (global-set-key (kbd "C-c g s") 'magit-status))
(safe-do-when magit-pull
  (global-set-key (kbd "C-c g p") 'magit-pull))
(safe-do-when magit-push
  (global-set-key (kbd "C-c g P") 'magit-push))
(safe-do-when magit-log
  (global-set-key (kbd "C-c g l") 'magit-log))
(safe-do-when magit-log-buffer-file
  (global-set-key (kbd "C-c g b") 'magit-log-buffer-file))
(safe-do-when magit-checkout
  (global-set-key (kbd "C-c g c") 'magit-checkout))
(safe-do-when magit-merge
  (global-set-key (kbd "C-c g m") 'magit-merge))

