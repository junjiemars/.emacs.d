;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;



(def-self-theme)

(comment
 (def-self-theme
   (emacs-home* "private/" "atom-one-dark-theme")
   'atom-one-dark))

(def-self-font "Monaco-18")
(def-self-cjk-font "Microsoft Yahei" 13)


(def-self-prelogue
  (message "#self prelogue ...")
  ;; (start-socks)
  ;; (setq debug-on-error t)

  )

(def-self-epilogue
  (message "#self epilogue ...")
  
  (safe-do-when org-agenda
    (global-set-key (kbd "C-c a") 'org-agenda))
  (safe-do-when org-capture
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

