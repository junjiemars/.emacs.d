;;;; -*- lexical-binding:t -*-
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;


(def-self-package-spec
  (list
   :cond `,(bin-exists-p "latex")
   :packages '(auctex cdlatex))
  (list
   :cond (and (version-supported-p <= 24.4)
              `,(bin-exists-p "java"))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking)
   :compile `(,(cons (emacs-home* "config/use-cider.el") t)
              ,(emacs-home* "config/use-cider-autoload.el")))
  (list
   :cond (and (version-supported-p <= 24.4)
              `,(bin-exists-p "docker"))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond `,(bin-exists-p "erlc")
   :packages '(erlang))
  (list
   :cond (and `,(bin-exists-p "erlc")
              `,(bin-exists-p "lfe"))
   :packages '(lfe-mode)
   :compile `(,(emacs-home* "config/use-lfe-autoload.el")))
  (list
   :cond (and (terminal-supported-p t)
              (platform-supported-unless darwin t)
              (version-supported-p <= 25.1))
   :packages '(ereader))
  (list
   :cond (and (version-supported-p <= 24.4)
              `,(bin-exists-p "git"))
   :packages '(magit)
   :compile `(,(emacs-home* "config/use-magit-autoload.el")))
  (list
   :cond (and (version-supported-p <= 23.2)
              (or `,(bin-exists-p "racket")
                  `,(bin-exists-p "chicken")))
   :packages '(geiser))
  (list
   :cond (or `,(bin-exists-p "sbcl"))
   :packages '(slime)
   :compile `(,(cons (emacs-home* "config/use-slime.el") t)
              ,(emacs-home* "config/use-slime-autoload.el")))
  )


