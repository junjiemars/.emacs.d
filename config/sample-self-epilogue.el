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


(safe-fn-when org-agenda
  (global-set-key (kbd "C-c a") 'org-agenda))
(safe-fn-when org-capture
  (global-set-key (kbd "C-c c") 'org-capture))

(comment (safe-setq enable-local-variables :all))
(comment (setq-default compilation-scroll-output t))

(comment
 (safe-fn-when semantic-mode
   (append-to-emacs-startup-hook
    (lambda ()
      (semantic-mode t)
      (declare-function semantic-reset-system-include "semantic")
      (declare-function semantic-add-system-include "semantic")
      (semantic-reset-system-include 'c-mode)
      (eval-when-compile (require 'cc))
      (dolist (x (system-cc-include t))
        (semantic-add-system-include x 'c-mode))
      (comment)
      (setq-default semanticdb-project-roots
                    '("/opt/apps/c"))))))

(comment
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq-default rmail-remote-password-required t))

(comment
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))
