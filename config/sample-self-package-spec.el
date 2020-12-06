;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;


(*self-packages*
 :put :doc
 (list
  :cond nil
  :packages (list (when% (executable-find% "gnuplot")
                    'gnuplot-mode)
                  'markdown-mode
                  'multiple-cursors
                  (when-version% <= 24.3 'yasnippet)
                  'vlf)))

(*self-packages*
 :put :org
 (list
  :cond nil
  :packages (flatten (list
                      (when% (executable-find% "latex")
                        '(auctex
                          cdlatex))
                      (when-version% <= 25 'ox-reveal)))))

(*self-packages*
 :put :vcs
 (list
  :cond (comment (and (when-version% <= 24.4 t)
                      (executable-find% "git")))
  :packages '(magit)
  :compile `(,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))

(*self-packages*
 :put :docker
 (list
  :cond (comment (and (when-version% <= 24.4 t)
                      (executable-find% "docker")))
  :packages '(dockerfile-mode)))

(*self-packages*
 :put :scheme
 (list
  :cond (comment (and (when-version% <= 23.2 t)
                      (or (executable-find% "racket")
                          (executable-find% "scheme")
                          (executable-find% "chicken")
                          (executable-find% "guile"))))
  :packages  '(geiser)
  :compile `(,(compile-unit% (emacs-home* "config/use-geiser-autoload.el")))))

(*self-packages*
 :put :common-lisp
 (list
  :cond (executable-find% "sbcl")
  :packages '(slime)
  :compile `(,(compile-unit% (emacs-home* "config/use-slime-autoload.el")))))

(*self-packages*
 :put :java
 (list
  :cond (comment (and (when-version% <= 25.1 t)
                      (executable-find% "java")))
  :packages '(cider
              clojure-mode
              clojure-mode-extra-font-locking
              kotlin-mode)
  :compile `(,(compile-unit% (emacs-home* "config/use-cider.el") t)
             ,(compile-unit%
               (emacs-home* "config/use-cider-autoload.el") t))))

(*self-packages*
 :put :erlang
 (list
  :cond (comment (executable-find% "erlc"))
  :packages (list 'erlang
                  (when% (executable-find% "lfe")
                    'lfe-mode))
  :compile (list (when% (executable-find% "lfe")
                   (compile-unit%
                    (emacs-home* "config/use-lfe-autoload.el"))))))

(*self-packages*
 :put :lua
 (list
  :cond (comment (executable-find% "lua"))
  :packages '(lua-mode)
  :compile `(,(compile-unit% (emacs-home* "config/use-lua-autoload.el")))))

(*self-packages*
 :put :web
 (list
  :cond nil
  :packages (list 'htmlize
                  'js2-mode
                  (when-version% <= 25 'restclient)
                  (when-version% <= 24.3 'skewer-mode)
                  'web-mode
                  'x509-mode)))

;; EOF
