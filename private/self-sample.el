;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;






(def-self-prelogue
  (message "#self prelogue ...")

  (comment (setq debug-on-error t)))


(def-self-epilogue
  (message "#self epilogue ...")
  
  (safe-fn-when org-agenda
    (global-set-key (kbd "C-c a") 'org-agenda))
  (safe-fn-when org-capture
    (global-set-key (kbd "C-c c") 'org-capture))
  
  (comment
   (version-supported-if
       <= 25.2
       (setq source-directory "/opt/open/emacs-25/")
     (setq source-directory "/opt/open/emacs-22/"))))


;; define env-spec
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "private/atom-one-dark-theme")
               :allowed nil)
  :font (list :name "Monaco-13"
              :allowed nil)
  :cjk-font (list :name "Microsoft Yahei"
                  :size 13
                  :allowed nil)
  :desktop (list :files-not-to-save "\.el\.gz\\|~$"
                 :allowed t)
  (comment
   :socks (list :port 11032
                :server "127.0.0.1"
                :version 5
                :allowed t)))


;; define package-spec
(def-self-env-spec
  :theme (list :name 'atom-one-dark
               :path (emacs-home* "private/atom-one-dark-theme")
               :allowed nil)
  :font (list :name "Monaco-13"
              :allowed nil)
  :cjk-font (list :name "Microsoft Yahei"
                  :size 13
                  :allowed nil)
  :desktop (list :files-not-to-save "\.el\.gz\\|~$"
                 :allowed t)
  (comment
   :socks (list :port 11032
                :server "127.0.0.1"
                :version 5
                :allowed t)))



