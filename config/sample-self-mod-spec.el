;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; sample-self-mod-spec.el: specify the module spec
;;
;;;;

;;; :common-lisp
(*self-mod-spec*
 :put :common-lisp
 `( :cond ,(comment (or (executable-find* "sbcl")
                        (executable-find* "ecl")
                        (executable-find* "acl")))
    :packages (slime)
    :compile (,(compile-unit* (emacs-home* "config/use-slime.el") t)
              ,(compile-unit* (emacs-home* "config/use-slime-autoload.el")))))

;;; :doc
(*self-mod-spec*
 :put :doc
 `( :cond nil
    :packages (,(when% (executable-find* "gnuplot") 'gnuplot-mode)
               ,(when-version% < 25.3 'yasnippet)
               ,(if-version%
                    <= 28.1
                    'markdown-mode
                  (if-version%
                      <= 27.1
                      `(markdown-mode
                        . ,(emacs-home* "scratch/markdown-mode-2.6/"))
                    `(markdown-mode
                      . ,(emacs-home* "scratch/markdown-mode-2.5.el"))))
               ,(when-version% < 25.3 'vlf))))

;;; :erlang
(*self-mod-spec*
 :put :erlang
 `( :cond ,(comment (executable-find* "erlc"))
    :packages (erlang)))

;;; :lisp
(*self-mod-spec*
 :put :lisp
 `( :cond ,(comment t)
    :packages (paredit rainbow-delimiters)
    :compile (,(compile-unit* (emacs-home* "config/use-lisp.el") t)
              ,(compile-unit* (emacs-home* "config/use-lisp-autoload.el")))))

;;; :lua
(*self-mod-spec*
 :put :lua
 `( :cond ,(comment (executable-find* "lua"))
    :packages (lua-mode)))

;;; :org
(*self-mod-spec*
 :put :org
 `( :cond ,(comment (executable-find* "latex"))
    :packages (auctex cdlatex ,(when-version% < 25 'ox-reveal))
    :compile (,(compile-unit* (emacs-home* "config/use-org.el") t)
              ,(compile-unit* (emacs-home* "config/use-org-autoload.el")))))

;;; :rust
(*self-mod-spec*
 :put :rust
 `( :cond ,(comment (and (executable-find* "rustc")
                         (executable-find* "cargo")))
    :packages (rust-mode)
    :compile (,(compile-unit* (emacs-home* "config/use-rust.el") t)
              ,(compile-unit* (emacs-home* "config/use-rust-autoload.el")))))

;;; :scheme
(*self-mod-spec*
 :put :scheme
 `( :cond ,(comment
            ;; Nore Emacs has builtin supports for Chez
            ;; scheme and gambitC scheme, and does not
            ;; need to install the dumb geiser.
            (or (executable-find* "racket")
                (executable-find* "chicken")
                (executable-find* "guile")))
    :packages  (geiser)
    :compile (,(compile-unit* (emacs-home* "config/use-geiser.el") t)
              ,(compile-unit* (emacs-home* "config/use-geiser-autoload.el")))))

;;; :swift
(*self-mod-spec*
 :put :swift
 `( :cond ,(comment (executable-find* "swift"))
    :packages (swift-mode)))

;;; :vlang
(*self-mod-spec*
 :put :vlang
 `( :cond ,(comment (executable-find* "v"))
    :packages (v-mode)))

;;; :vcs
(*self-mod-spec*
 :put :vcs
 `( :cond ,(comment (when-version% <= 27.1 (executable-find* "git")))
    :packages ,(prog1 '(magit)
                 (set-default 'magit-define-global-key-bindings nil))
    :compile (,(compile-unit* (emacs-home* "config/use-magit.el") t)
              ,(compile-unit* (emacs-home* "config/use-magit-autoload.el")))))

;;; :web
(*self-mod-spec*
 :put :web
 `( :cond nil
    :packages (htmlize
               js2-mode
               ,(when-version% <= 25 'restclient)
               ,(when-version% <= 24.3 'skewer-mode)
               web-mode
               x509-mode)))

;;; :zig
(*self-mod-spec*
 :put :zig
 `( :cond ,(comment (executable-find* "zig"))
    :packages (zig-mode)))


;; end of sample-self-package-spec.el
