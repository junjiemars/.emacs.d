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
   :cond nil
   :packages (list 'vlf
                   'x509-mode
                   ;; (when-version% <= 25 'ox-reveal)
                   (when-version% <= 24.3 'yasnippet)))
  (list
   :cond nil ;; (when-version% < 24.1 t)
   :packages (list 'js2-mode
                   'web-mode
                   'skewer-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-js-autoload.el"))))
  (list
   :cond (executable-find% "latex")
   :packages '(auctex cdlatex))
  (list
   :cond (and (when-version% <= 25.1 t)
              (executable-find% "java"))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking
               kotlin-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-cider.el") t)
              ,(compile-unit% (emacs-home* "config/use-cider-autoload.el"))))
  (list
   :cond (and (when-version% <= 24.4 t)
              (executable-find% "docker"))
   :packages '(dockerfile-mode))
  (list
   :cond (executable-find% "erlc")
   :packages '(erlang))
  (list
   :cond (and (executable-find% "erlc")
              (executable-find% "lfe"))
   :packages '(lfe-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-lfe-autoload.el"))))
  (list
   :cond (and (unless-graphic% t)
              (unless-platform% 'darwin t)
              (when-version% <= 25.1))
   :packages '(ereader))
  (list
   :cond (and (when-version% <= 25.1 t)
              (executable-find% "git"))
   :packages '(magit)
   :compile `(,(compile-unit% (emacs-home* "config/use-magit-autoload.el"))))
  (list
   :cond (and (when-version% <= 23.2 t)
              (or (executable-find% "racket")
                  (executable-find% "chicken")))
   :packages '(geiser))
  (list
   :cond (or (executable-find% "sbcl"))
   :packages '(slime)
   :compile `(,(compile-unit% (emacs-home* "config/use-slime-autoload.el"))))
  (list
   :cond (executable-find% "lua")
   :packages '(lua-mode)
   :compile `(,(compile-unit% (emacs-home* "config/use-lua-autoload.el"))))

  ) ;; end of `def-self-package-spec'

