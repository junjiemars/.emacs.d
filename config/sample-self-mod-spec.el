;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;

;;; :common-lisp
(*self-mod-spec*
 :put :common-lisp
 (list
  :cond (comment (or (executable-find% "sbcl")
                     (executable-find% "ecl")
                     (executable-find% "acl")))
  :packages '(slime)
  :compile `(,(compile-unit% (emacs-home* "config/use-slime.el") t)
             ,(compile-unit% (emacs-home* "config/use-slime-autoload.el")))))

;;; :doc
(*self-mod-spec*
 :put :doc
 (list
  :cond nil
  :packages (list (when% (executable-find% "gnuplot")
                    'gnuplot-mode)
                  'markdown-mode
                  (when-version% <= 24.3 'yasnippet)
                  'vlf)))

;;; :erlang
(*self-mod-spec*
 :put :erlang
 (list
  :cond (comment (executable-find% "erlc"))
  :packages (list 'erlang)))

;;; :lisp
(*self-mod-spec*
 :put :lisp
 (list
  :cond (comment t)
  :packages '(paredit rainbow-delimiters)
  :compile `(,(compile-unit% (emacs-home* "config/use-lisp.el") t)
             ,(compile-unit% (emacs-home* "config/use-lisp-autoload.el")))))

;;; :lua
(*self-mod-spec*
 :put :lua
 (list
  :cond (comment (executable-find% "lua"))
  :packages '(lua-mode)))

;;; :org
(*self-mod-spec*
 :put :org
 (list
  :cond (comment (executable-find% "latex"))
  :packages (flatten (list 'auctex 'cdlatex
                           (when-version% <= 25 'ox-reveal)))))

;;; :rust
(*self-mod-spec*
 :put :rust
 (list
  :cond (comment (and (executable-find% "rustc")
                      (executable-find% "cargo")))
  :packages '(rust-mode)
  :compile `(,(compile-unit% (emacs-home* "config/use-rust.el") t)
             ,(compile-unit% (emacs-home* "config/use-rust-autoload.el")))))

;;; :scheme
(*self-mod-spec*
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
(*self-mod-spec*
 :put :vlang
 (list
  :cond (comment (executable-find% "v"))
  :packages '(v-mode)))

;;; :vcs
(*self-mod-spec*
 :put :vcs
 (list
  :cond (comment (and (when-version% <= 24.4 t)
                      (executable-find% "git")))
  :packages (prog1 '(magit)
              (set-default 'magit-define-global-key-bindings nil))
  :compile `(,(compile-unit% (emacs-home* "config/use-magit.el") t)
             ,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))

;;; :web
(*self-mod-spec*
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
(*self-mod-spec*
 :put :zig
 (list
  :cond (comment (executable-find% "zig"))
  :packages '(zig-mode)))


;; end of sample-self-package-spec.el
