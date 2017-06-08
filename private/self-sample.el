;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;



(def-self-env-spec
  (list
   :theme (list :name 'atom-one-dark
                :path (emacs-home* "private/atom-one-dark-theme")
                :allowed nil)
   :font (list :name "Monaco-13"
               :allowed nil)
   :cjk-font (list :name "Microsoft Yahei"
                   :size 13
                   :allowed nil)
   :socks (list :port 11032
                :server "127.0.0.1"
                :version 5
                :allowed nil)))


(platform-supported-when
    windows-nt
  (def-self-cjk-font "Microsoft Yahei" 13))

;; (def-self-socks 32000)

(def-self-prelogue
  (message "#self prelogue ...")

  ;; (setq debug-on-error t)

  )

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
     (setq source-directory "/opt/open/emacs-22/")))
  )


(def-self-package-spec
  (list
   :cond (lambda ()
           (bin-exists-p "latex"))
   :packages '(auctex cdlatex))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (bin-exists-p "java")))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking
               inf-clojure)
   :setup '("setup-clojure.el"))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (bin-exists-p "docker")))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond (lambda ()
           (bin-exists-p "erlang"))
   :packages '(erlang))
  (list
   :cond (lambda ()
           (and (bin-exists-p "erlang")
                (bin-exists-p "lfe")))
   :packages '(lfe-mode)
   :setup '("setup-lfe.el"))
  (list
   :cond (lambda ()
           (version-supported-p '<= 25.1))
   :packages '(ereader))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (bin-exists-p "git")))
   :packages '(magit)
   :setup '("setup-magit.el"))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 23.2)
                (bin-exists-p "racket")))
   :packages '(geiser))
  (list
   :cond (lambda ()
           (or (bin-exists-p "sbcl")))
   :packages '(slime)
   :setup '("setup-slime.el"))
  (list
   :cond (lambda () t)
   :packages '(sx)))

