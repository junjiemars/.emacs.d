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

(comment (safe-setq enable-local-variables :all))
(comment (setq-default compilation-scroll-output t))

(comment
 (safe-fn-when semantic-mode
   (add-hook 'after-init-hook
             (lambda ()
               (semantic-mode t)
               (eval-when-compile (require 'semantic/dep))
               (semantic-reset-system-include 'c-mode)
               (dolist (x (system-cc-include-paths t))
                 (semantic-add-system-include x 'c-mode))
               (comment
                (setq-default semanticdb-project-roots
                              '("/opt/apps/c"))))
             t)))

(comment
 (version-supported-if
     <= 25.2
     (setq source-directory "/opt/open/emacs-25/")
   (setq source-directory "/opt/open/emacs-22/")))


(comment
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq-default rmail-remote-password-required t))

(comment
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))
