;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;

;;; :common-lisp
(*self-packages*
 :put :common-lisp
 (list
  :cond (comment (or (executable-find% "sbcl")
                     (executable-find% "ecl")
                     (executable-find% "acl")))
  :packages '(slime)
  :compile `(,(compile-unit% (emacs-home* "config/use-slime.el") t)
             ,(compile-unit% (emacs-home* "config/use-slime-autoload.el")))))

;;; :doc
(*self-packages*
 :put :doc
 (list
  :cond nil
  :packages (list (when% (executable-find% "gnuplot")
                    'gnuplot-mode)
                  'markdown-mode
                  (when-version% <= 24.3 'yasnippet)
                  'vlf)))

;;; :erlang
(*self-packages*
 :put :erlang
 (list
  :cond (comment (executable-find% "erlc"))
  :packages (list 'erlang)))

;;; :lisp
(*self-packages*
 :put :lisp
 (list
  :cond (comment t)
  :packages '(paredit rainbow-delimiters)
  :compile `(,(compile-unit% (emacs-home* "config/use-lisp.el") t)
             ,(compile-unit% (emacs-home* "config/use-lisp-autoload.el")))))

;;; :lua
(*self-packages*
 :put :lua
 (list
  :cond (comment (executable-find% "lua"))
  :packages '(lua-mode)))

;;; :org
(*self-packages*
 :put :org
 (list
  :cond nil
  :packages (flatten (list (when% (executable-find% "latex")
                             '(auctex
                               cdlatex))
                           (when-version% <= 25 'ox-reveal)))))

;;; :rust
(*self-packages*
 :put :rust
 (list
  :cond (comment (and (executable-find% "rustc")
                      (executable-find% "cargo")))
  :packages '(rust-mode)
  :compile `(,(compile-unit% (emacs-home* "config/use-rust.el") t)
             ,(compile-unit% (emacs-home* "config/use-rust-autoload.el")))))

;;; :scheme
(*self-packages*
 :put :scheme
 (list
  :cond (comment (and (when-version% <= 23.2 t)
                      ;; Nore Emacs has builtin supports for Chez
                      ;; scheme and gambitC scheme, and does not need to
                      ;; install the dumb geiser.
                      (or (executable-find% "racket")
                          (executable-find% "scheme")
                          (executable-find% "chicken")
                          (executable-find% "guile"))))
  :packages  '(geiser)
  :compile `(,(compile-unit% (emacs-home* "config/use-geiser.el") t)
             ,(compile-unit% (emacs-home* "config/use-geiser-autoload.el")))))

;;; :vlang
(*self-packages*
 :put :vlang
 (list
  :cond (comment (executable-find% "v"))
  :packages '(v-mode)))

;;; :vcs
(*self-packages*
 :put :vcs
 (list
  :cond (comment (and (when-version% <= 24.4 t)
                      (executable-find% "git")))
  :packages '(magit)
  :compile `(,(compile-unit% (emacs-home* "config/use-magit.el") t)
             ,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))

;;; :web
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

;;; :zig
(*self-packages*
 :put :zig
 (list
  :cond (comment (executable-find% "zig"))
  :packages '(zig-mode)))


;; end of sample-self-package-spec.el
