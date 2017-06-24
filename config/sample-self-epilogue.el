;;;;
;; sample-self-epilogue.el: specify the epilogue of yourself
;;   define self-epilogue, it will be run the last end
;;
;;;;



(message "#self epilogue ...")

(safe-fn-when org-agenda
  (global-set-key (kbd "C-c a") 'org-agenda))
(safe-fn-when org-capture
  (global-set-key (kbd "C-c c") 'org-capture))

(comment
 (version-supported-if
     <= 25.2
     (setq source-directory "/opt/open/emacs-25/")
   (setq source-directory "/opt/open/emacs-22/")))

