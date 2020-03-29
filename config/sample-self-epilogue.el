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

(comment (setq% enable-local-variables :all 'files))



;; eww as default browser
(comment (if-feature-eww% (toggle-browser! t)))

;; open-line indent
(toggle-open-line-indent! t)




;;;; tag

(comment
 ;; using etags to view Emacs's source code: C and Elisp
 (add-to-list 'tags-in-view-mode
              (setq source-directory
                    (let ((srcdir (format "/opt/open/emacs-%s/"
                                          emacs-major-version)))
                      (when (file-exists-p srcdir)
                        srcdir)))
              nil #'string=))

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
 
 (setq% sql-connection-alist '((mysql-local
                                (sql-product 'mysql)
                                (sql-server "127.0.0.1")
                                (sql-port 3306)
                                (sql-database "sys")
                                (sql-user "root")
                                (sql-password "example")))
        'sql))

 ;; end of sql


;;;; snippet

(comment
 (setq% yas-snippet-dirs '("<where>") 'yasnippet))

 ;; end of snippet


;;;; web

(comment
 ;; httpd: M-x httpd-start
 ;; skewer: M-x run-skewer

 (setq% httpd-root
        (path! (emacs-home* "private/httpd"))
        'simple-httpd)
 (setq% httpd-port 8080 'simple-httpd)

 ;; web-mode, http://web-mode.org
 (when-var% web-mode-hook 'web-mode
   (add-hook 'web-mode-hook
             #'(lambda ()
                 (setq web-mode-comment-formats
                       '(("java" . "/*")
                         ("javascript" . "//")
                         ("php" . "/*")
                         ("css" . "/*")))
                 (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
                 (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))))))

 ;; end of web


;; end of file
