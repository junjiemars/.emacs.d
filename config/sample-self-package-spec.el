;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;


(def-self-package-spec
  (list
   :cond t
   :packages '(markdown-mode htmlize org-tree-slide))
  (list
   :cond (executable-find% "latex")
   :packages '(auctex cdlatex))
  (list
   :cond (and (version-supported-p <= 25.1)
              (executable-find% "java"))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking)
   :compile `(,(compile-unit% (emacs-home* "config/use-cider.el") t)
              ,(compile-unit% (emacs-home* "config/use-cider-autoload.el"))))
  (list
   :cond (and (version-supported-p <= 24.4)
              (executable-find% "docker"))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond (executable-find% "erlc")
   :packages '(erlang))
  (list
   :cond (and (executable-find% "erlc")
              (executable-find% "lfe"))
   :packages '(lfe-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-lfe-autoload.el"))))
  (list
   :cond (and (terminal-supported-p t)
              (platform-supported-unless 'darwin t)
              (version-supported-p <= 25.1))
   :packages '(ereader))
  (list
   :cond (and (version-supported-p <= 24.4)
              (executable-find% "git"))
   :packages '(magit)
   :compile `(,(compile-unit% (emacs-home* "config/use-magit-autoload.el"))))
  (list
   :cond (and (version-supported-p <= 23.2)
              (or (executable-find% "racket")
                  (executable-find% "chicken")))
   :packages '(geiser))
  (list
   :cond (or (executable-find% "sbcl"))
   :packages '(slime)
   :compile `(,(compile-unit% (emacs-home* "config/use-slime.el"))
              ,(compile-unit% (emacs-home* "config/use-slime-autoload.el"))))
  )


