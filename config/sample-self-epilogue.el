;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-epilogue.el: specify the epilogue of yourself
;;   should be run on the end of Emacs init 
;;
;;;;



(message "#self epilogue ...")


;; safe local variables
(comment (safe-local-variable* 'indent-tabs))


;; (require 'financial)

(comment (setq% enable-local-variables :all 'files))



;; eww as default browser
(comment (if-feature-eww% (toggle-browser! t)))




;;;; tag

(comment
 ;; using etags to view Emacs's source code: C and Elisp
 (let ((srcdir (format "/opt/open/emacs-%s/"
                       emacs-major-version)))
   (when (file-exists-p srcdir) srcdir
         (tags-in-view-mode (setq source-directory srcdir))
         (setq% find-function-C-source-directory
                (concat source-directory "src/") 'find-func))))

 ;; end of tag


;;;; org

(comment
 (with-eval-after-load 'org
   ;; define key bindings after `org-mode' had been loaded
   ;; publish blog
   (when-var% org-publish-project-alist 'org
     (setq
      org-publish-project-alist
      '(("blog"
         :base-directory "<your-blog-dir>"
         :base-extension "org"
         :publishing-directory "<public/blog/>"
         :recursive t
         :publishing-function org-html-publish-to-html)
        ("website" :components ("blog")))))))

 ;; end of org


;;;; slime

(comment
 (defmacro-if-feature% slime)
 (if-feature-slime% (*slime-source-locations* "<source-dir>")))

 ;; end of slime


;;;; c/c++

(comment
 ;; if current Emacs session support `semantic-mode'
 ;; using semantic to view and editing any supported code
 ;; correctly and more faster
 (if-feature-semantic%
     (add-hook 'semantic-mode-hook
               #'(lambda ()
                   (if-platform% 'windows-nt
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

 ;; end of c/c++


;;;; mail

(comment
 ;; receive mail
 (require 'rmail)
 (setq rmail-primary-inbox-list '("<protocal://user:passwd@host>"))
 (setq% rmail-remote-password-required t 'rmail)
 ;; send mail
 (require 'sendmail)
 (setq send-mail-function 'smtpmail-send-it)
 (setq smtpmail-smtp-server "<smtp-server>")
 (setq smtpmail-smtp-server 587))

 ;; end of mail


;;;; sql
(comment

 (unless% (executable-find% "mysql")
   (setq% sql-mysql-program
          (if-platform% 'darwin
              "/Applications/MySQLWorkbench.app/Contents/MacOS/mysql"
            "<where>")
          'sql))
 
 (setq% sql-connection-alist
        '((mysql-local
           (sql-product 'mysql)
           (sql-server "127.0.0.1")
           (sql-port 3306)
           (sql-database "sys")
           (sql-user "root")
           (sql-password "example"))
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

;;;;
;; Snippet
;;;;

(comment
 ;; (setq yas-indent-line 'auto 'yasnippet)
 (setq% yas-snippet-dirs '("<where>") 'yasnippet))

 ;; end of snippet

;;;;
;; Web
;;;;

(comment
 ;; httpd: M-x httpd-start
 ;; skewer: M-x run-skewer

 (setq% httpd-root
        (path! (emacs-home* "private/httpd"))
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


;; end of file
