;;;; -*- lexical-binding:t -*-
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;


(def-self-package-spec
  (list
   :cond (bin-exists-p "latex")
   :packages '(auctex cdlatex))
  (list
   :cond (and (version-supported-p <= 24.4)
              (platform-supported-if
                  darwin
                  (zerop (shell-command
                          "/usr/libexec/java_home -V &>/dev/null"))
                (bin-exists-p "java")))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking)
   :compile `(,(emacs-home* "config/setup-clojure.el")))
  (list
   :cond (and (version-supported-p <= 24.4)
              (bin-exists-p "docker"))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond (bin-exists-p "erlc")
   :packages '(erlang))
  (list
   :cond (and (bin-exists-p "erlc")
              (bin-exists-p "lfe"))
   :packages '(lfe-mode)
   :compile `(,(emacs-home* "config/use-lfe-autoload.el")))
  (list
   :cond (and (terminal-supported-p t)
              (platform-supported-unless darwin t)
              (version-supported-p <= 25.1))
   :packages '(ereader))
  (list
   :cond (and (version-supported-p <= 24.4)
              (bin-exists-p "git"))
   :packages '(magit)
   :compile `(,(cons (emacs-home* "config/use-magit.el") t)
              (emacs-home* "config/use-magit-autoload.el")))
  (list
   :cond (and (version-supported-p <= 23.2)
              (or (bin-exists-p "racket")
                  (bin-exists-p "chicken")))
   :packages '(geiser))
  (list
   :cond (or (bin-exists-p "sbcl"))
   :packages '(slime)
   :compile `(,(cons (emacs-home* "config/use-slime.el") t)
              (emacs-home* "config/use-slime-autoload.el")))
  (list
   :cond (version-supported-p <= 24.1)
   :packages '(yaml-mode))
  (list
   :cond (version-supported-p <= 24.4)
   :packages '(groovy-mode)))


