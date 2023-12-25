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
  :compile `(,(compile-unit% (emacs-home* "config/use-slime-autoload.el")))))

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

;; `eglot' package for ancient Emacs
(*self-packages*
 :put :eglot
 (list
  :cond (comment t)
  :packages `(,(when-version% > 29 'eglot))
  :compile `(,(compile-unit% (emacs-home* "config/on-eglot-autoload.el")))))

;;; :erlang
(*self-packages*
 :put :erlang
 (list
  :cond (comment (executable-find% "erlc"))
  :packages (list 'erlang
                  (when% (executable-find% "lfe")
                    'lfe-mode))
  :compile (list (when% (executable-find% "lfe")
                   (compile-unit%
                    (emacs-home* "config/use-lfe-autoload.el") t)))))

;;; :lisp
(*self-packages*
 :put :lisp
 (list
  :cond (comment t)
  :packages '(paredit rainbow-delimiters)
  :compile `(,(compile-unit% (emacs-home* "config/use-lisp-autoload.el")))))

;;; :lua
(*self-packages*
 :put :lua
 (list
  :cond (comment (executable-find% "lua"))
  :packages '(lua-mode)
  :compile `(,(compile-unit% (emacs-home* "config/use-lua-autoload.el") t))))

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
  :packages '(rust-mode)))

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
  :compile `(,(compile-unit% (emacs-home* "config/use-geiser-autoload.el")))))

;;; :vlang
(*self-packages*
 :put :vlang
 (list
  :cond (comment (executable-find% "v"))
  :packages '(v-mode)))

;;; :treesit package for ancient Emacs
(*self-packages*
 :put :treesit
 (list
  :cond (comment t)
  :packages (list (when-version% > 29 'treesit))
  :compile `(,(compile-unit% (emacs-home* "config/on-treesit-autoload.el")))))

;;; :vcs
(*self-packages*
 :put :vcs
 (list
  :cond (comment (and (when-version% <= 24.4 t)
                      (executable-find% "git")))
  :packages '(magit)
  :compile `(,(compile-unit% (emacs-home* "config/use-magit-autoload.el")))))

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
