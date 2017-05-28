;;;;
;; self-sample.el: specified yourself private configuration elisp file
;;                 and named it with self.el
;;;;


(def-self-font "Monaco-12")
;; (def-self-font "Consolas-13")
;; (def-self-font "White Rabbit-12")

(def-self-cjk-font (cons "Microsoft Yahei" 13))

(def-self-theme 'tomorrow-night-eighties)


(def-self-prelogue
  (message "#self prelogue ...")
  ;; (start-socks)
  ;; (setq debug-on-error t)
  (self-install-package)
  ;; (setq source-directory "/opt/open/emacs/")
  )

(def-self-epilogue
  (message "#self epilogue ...")
  (safe-do-when org-agenda
    (global-set-key (kbd "C-c a") 'org-agenda))
  (safe-do-when org-capture
    (global-set-key (kbd "C-c c") 'org-capture)))


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


