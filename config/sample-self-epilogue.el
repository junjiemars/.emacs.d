;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-epilogue.el: specify the epilogue of yourself
;;   should be run on the end of Emacs init
;;
;;;;



;; (message "#self epilogue ...")

;; (setq% enable-local-variables :safe 'files)

;; (require 'financial)



;; eww as default browser
;; (if-feature-eww% (toggle-browser! t)))

;; end of eww


;;; tag

(comment
 ;; using etags to view Emacs's source code: C and Elisp
 (let ((srcdir (format "/opt/open/emacs-%s/"
                       emacs-major-version)))
   (when (file-exists-p srcdir)
     (tags-in-view-mode (setq source-directory srcdir))
     (setq% find-function-C-source-directory
            (concat source-directory "src/") 'find-func))))

;; end of tag


;;; org

(comment
 (with-eval-after-load 'org
   ;; define key bindings after `org-mode' had been loaded
   ;; publish blog
   (when-fn% 'org-babel-do-load-languages 'org
     (org-babel-do-load-languages
      'org-babel-load-languages
      (delq nil `(,(when-var% +cc*-compiler-bin+ nil
                     (setq% org-babel-C-compiler
                            +cc*-compiler-bin+ 'ob-C)
                     `(C . t))
                  (emacs-lisp . t)
                  ,(when% (require 'ob-shell nil t)
                     `(shell . t))))))))

;; end of org


;;; slime

(comment
 (defmacro-if-feature% slime)
 (if-feature-slime%
     (when-fn% '*slime-source-locations* nil
       (*slime-source-locations* "<source-dir>"))))

;; end of slime

;;; mail

;; (comment
;;  ;; receive mail
;;  (require 'rmail)
;;  (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
;;  (setq% rmail-remote-password-required t 'rmail)
;;  ;; send mail
;;  (require 'sendmail)
;;  (setq send-mail-function 'smtpmail-send-it)
;;  (setq smtpmail-smtp-server "<smtp-server>")
;;  (setq smtpmail-smtp-server 587))

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
           ;;    can be set via :spin-vars or :copy-vars in self-env-spec.el
           ;; 3. (sql-database "<HOST>/SID")
           (sql-product 'oracle)
           (sql-server "127.0.0.1")
           (sql-port 1521)
           (sql-database "127.0.0.1/XE")
           (sql-user "system")
           (sql-password "oracle")))
        'sql))

;; end of sql

;;; snippet

(comment
 ;; (setq yas-indent-line 'auto 'yasnippet)
 (setq% yas-snippet-dirs '("<where>") 'yasnippet))

;; end of snippet


;;; web

(comment
 ;; httpd: M-x httpd-start
 ;; skewer: M-x run-skewer

 (setq% httpd-root
        (path! (emacs-home* "private/httpd/"))
        'simple-httpd)
 (setq% httpd-port 8080 'simple-httpd)

 ;; web-mode, http://web-mode.org
 (defmacro-if-feature% web-mode)
 (if-feature-web-mode%
     (progn%
      (push! '("\\.html\\'" . web-mode) auto-mode-alist)
      (push! '("\\.vue\\'" . web-mode) auto-mode-alist )
      (add-hook 'web-mode-hook
                #'(lambda ()
                    (setq web-mode-comment-formats
                          '(("java" . "/*")
                            ("javascript" . "//")
                            ("php" . "/*")
                            ("css" . "/*"))))))))

;; end of web


;; end of sample-self-epilogue.el
