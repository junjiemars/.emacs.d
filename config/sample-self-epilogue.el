;;;; -*- lexical-binding:t -*-
;;;;
;; sample-self-epilogue.el: specify the epilogue of yourself
;;   define self-epilogue, it will be run the last end
;;
;;;;



(message "#self epilogue ...")


;; (require 'financial)
;; (require 'shells)
;; (require 'eshells)
;; (require 'pythons)


(comment
 (when (require 'tags)
   (version-supported-if
       <= 25.2
       (setq source-directory "/opt/open/emacs-25/")
     (setq source-directory "/opt/open/emacs-22/"))))


(with-eval-after-load 'org
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o c") 'org-capture))


(comment (safe-setq enable-local-variables :all))
(comment (setq-default compilation-scroll-output t))

(comment
 (safe-fn-when semantic-mode
   (add-hook
    'after-init-hook
    (lambda ()
      (semantic-mode t)
      (declare-function semantic-reset-system-include "semantic")
      (declare-function semantic-add-system-include "semantic")
      (declare-function global-semantic-idle-summary-mode "semantic")
      (semantic-reset-system-include 'c-mode)
      (global-semantic-idle-summary-mode)
      (eval-when-compile (require 'cc))
      (dolist (x (system-cc-include t))
        (semantic-add-system-include x 'c-mode))
      (comment)
      (setq-default semanticdb-project-roots
                    '("/opt/apps/c")))
    t)))

(comment
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq-default rmail-remote-password-required t))

(comment
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))
