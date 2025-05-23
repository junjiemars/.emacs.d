;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-epilogue.el: specify the epilogue spec,
;;   should be run on the end of Emacs init
;;
;;;;



;; (message "#self epilogue ...")

(comment
 (require 'financial))



;; eww as default browser
(comment
 (when-feature% eww (toggle-browser! t)))

;; end of eww


;;; using `tags' to view Emacs's source code

(comment
 (let ((srcdir (format "/opt/open/emacs/emacs-%s/" emacs-major-version)))
   (when (file-exists-p srcdir)
     (xref*-read-only-dirs :push (setq source-directory srcdir))
     (setq% find-function-C-source-directory
            (concat source-directory "src/") find-func))))

;; end of tag


;;; org

(comment
 (with-eval-after-load 'org
   ;; define key bindings after `org-mode' had been loaded
   ;; publish blog
   (when-fn% org-babel-do-load-languages org
     (org-babel-do-load-languages
      'org-babel-load-languages
      (delq nil `(,(when-fn% cc*-cc cc
                     (setq% org-babel-C-compiler (cc*-cc) ob-C)
                     `(C . t))
                  (emacs-lisp . t)
                  ,(when% (require 'ob-shell nil t)
                     `(shell . t))))))))

;; end of org


;;; slime

(comment
 (when-feature% slime
   (when-fn% slime*-source-locations use-slime
     (slime*-source-locations "/opt/open/lisp/"))))

;; end of slime

;;; mail

(comment
 ;; receive mail
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq% rmail-remote-password-required t rmail)
 ;; send mail
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))

;; end of mail


;;; sql

(comment
 (setq% sql-connection-alist
        '((mysql-local
           (sql-product 'mysql)
           (sql-server "127.0.0.1")
           (sql-port 3306)
           (sql-database "sys")
           (sql-user "root")
           (sql-password "example")
           (sql-mysql-options '("--local-infile=1")))
          (oracle-local
           ;; 1. install sql*plus instant client
           ;; 2. set environment variables: ORACLE_HOME, LD_LIBRARY_PATH
           ;;    via :spin-vars or :copy-vars in self-env-spec.el
           ;; 3. (sql-database "<HOST>/SID")
           (sql-product 'oracle)
           (sql-server "127.0.0.1")
           (sql-port 1521)
           (sql-database "127.0.0.1/XE")
           (sql-user "system")
           (sql-password "oracle"))
          (oceanbase-local
           (sql-product 'oceanbase)
           (sql-server "127.0.0.1")
           (sql-port 2883)
           (sql-database "example")
           (sql-user "ocean@example")
           (sql-password "example")))
        sql))

;; end of sql

;;; snippet

(comment
 ;; (setq yas-indent-line 'auto 'yasnippet)
 (setq% yas-snippet-dirs '("<where>") yasnippet))

;; end of snippet


;;; web

(comment
 ;; httpd: M-x httpd-start
 ;; skewer: M-x run-skewer
 (setq% httpd-root (path! (emacs-home% "private/httpd/")) simple-httpd)
 (setq% httpd-port 8080 simple-httpd)
 ;; web-mode, http://web-mode.org
 (when-feature% web-mode
   (push! '("\\.html\\'" . web-mode) auto-mode-alist)
   (push! '("\\.vue\\'" . web-mode) auto-mode-alist )
   (add-hook 'web-mode-hook
             #'(lambda ()
                 (setq web-mode-comment-formats
                       '(("java" . "/*")
                         ("javascript" . "//")
                         ("php" . "/*")
                         ("css" . "/*")))))))

;; end of web


(when-interactive%
  (message "Elapsed %s" (emacs-init-time)))

;; end of sample-self-epilogue.el
