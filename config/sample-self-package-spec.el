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
   :packages (list (when% (executable-find% "gnuplot")
                     'gnuplot-mode)
                   'markdown-mode
                   'multiple-cursors
                   'vlf))
  (list
   ;; org
   :cond t
   :packages (flatten (list
                       (when% (executable-find% "latex")
                         '(auctex
                           cdlatex))
                       (when-version% <= 25 'ox-reveal))))
  (list
   :cond (and (when-version% <= 24.4 t)
              (executable-find% "git"))
   :packages '(magit)
   :compile `(,(compile-unit% (emacs-home* "config/use-magit-autoload.el"))))
  (list
   ;; docker
   :cond (and (when-version% <= 24.4 t)
              (executable-find% "docker"))
   :packages '(dockerfile-mode))
  (list
   ;; scheme
   :cond (and (when-version% <= 23.2 t)
              (or (executable-find% "racket")
                  (executable-find% "chicken")))
   :packages  '(geiser)
   :compile `(,(compile-unit% (emacs-home* "config/use-geiser-autoload.el"))))
  (list
   ;; common lisp
   :cond (executable-find% "sbcl")
   :packages '(slime)
   :compile `(,(compile-unit% (emacs-home* "config/use-slime-autoload.el"))))
  (list
   ;; java
   :cond (and (when-version% <= 25.1 t)
              (executable-find% "java"))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking
               kotlin-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-cider.el") t)
              ,(compile-unit%
                (emacs-home* "config/use-cider-autoload.el") t)))
  (list
   ;; erlang
   :cond (executable-find% "erlc")
   :packages (list 'erlang
                   (when% (executable-find% "lfe")
                     'lfe-mode))
   :compile (list (when% (executable-find% "lfe")
                    (compile-unit%
                     (emacs-home* "config/use-lfe-autoload.el")))))
  (list
   ;; lua
   :cond (executable-find% "lua")
   :packages '(lua-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-lua-autoload.el"))))
  (list
   ;; web
   :cond nil
   :packages (list 'htmlize
                   'js2-mode
                   (when-version% <= 25 'restclient)
                   (when-version% <= 24.3 'skewer-mode)
                   'web-mode
                   'x509-mode
                   (when-version% <= 24.3 'yasnippet)))

  ) ;; end of def-self-package-spec


;; EOF
