;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-epilogue.el: specify the epilogue of yourself
;;   should be run on the end of Emacs init 
;;
;;;;



(message "#self epilogue ...")


;; (require 'financial)
;; (require 'shells)
;; (require 'eshells)
;; (require 'pythons)


;; eww as default browser
(feature-eww-supported-p (toggle-browser! t))


(with-eval-after-load 'org
  ;; define key bindings after `org-mode' had been loaded
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (global-set-key (kbd "C-c o c") 'org-capture))


(comment (setq% enable-local-variables :all 'files))

(comment
 ;; using etags to view Emacs's source code: C and Elisp
 ;; support any platform
 (setq source-directory
       (let ((srcdir (format "/opt/open/emacs-%s/"
           emacs-major-version)))
   (when (file-exists-p srcdir)
     srcdir))))

(comment
 ;; if current Emacs session support `semantic-mode'
 ;; using semantic to view and editing any supported code
 ;; correctly and more faster
 ;; `system-cc-include' support any platform
 (feature-semantic-supported-p
   (add-hook 'semantic-mode-hook
						 #'(lambda ()
								 (platform-supported-if 'windows-nt
										 (set-semantic-cc-env!
											`("d:/opt/open/ecl/"
												"d:/opt/open/ecl/build/"
												"d:/opt/open/gambit/"
												"e:/apps/c/out/"
												"e:/apps/c/src/hi/"
												"e:/apps/c/src/io/"
												"e:/apps/c/src/lang/"
												"e:/apps/c/src/memory/"
												"e:/apps/c/src/posix/"
												,source-directory)
											`("e:/apps/c/"
												,source-directory))
									 (set-semantic-cc-env!
										`("/opt/apps/c/out/"
											"/opt/apps/c/src/hi/"
											"/opt/apps/c/src/io/"
											"/opt/apps/c/src/lang/"
											"/opt/apps/c/src/memory/"
											"/opt/apps/c/src/posix/"
											"/opt/open/ecl/build/"
											"/opt/open/gambit/"
											"/opt/opt/open/ecl/"
											,source-directory)
										`("/opt/apps/c/"
											,source-directory))))
						 t)))


(comment
 ;; receive mail
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq% rmail-remote-password-required t 'rmail))

(comment
 ;; send mail
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))

