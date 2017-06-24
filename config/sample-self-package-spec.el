;;;;
;; sample-self-package-spec.el: specify the package spec of yourself
;;
;;;;


(def-self-package-spec
  (list
   :cond (lambda ()
           (bin-exists-p "latex"))
   :packages '(auctex cdlatex))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (platform-supported-if
                    darwin
                    (zerop (shell-command
                            "/usr/libexec/java_home -V &>/dev/null"))
                  (bin-exists-p "java"))))
   :packages '(cider
               clojure-mode
               clojure-mode-extra-font-locking)
   :compile `(,(emacs-home* "config/setup-clojure.el")))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (bin-exists-p "docker")))
   :packages '(dockerfile-mode
               docker-tramp))
  (list
   :cond (lambda ()
           (bin-exists-p "erlc"))
   :packages '(erlang))
  (list
   :cond (lambda ()
           (and (bin-exists-p "erlc")
                (bin-exists-p "lfe")))
   :packages '(lfe-mode)
   :compile `(,(emacs-home* "config/setup-lfe.el")))
  (list
   :cond (lambda ()
           (and (terminal-supported-p t)
                (platform-supported-unless darwin t)
                (version-supported-p '<= 25.1)))
   :packages '(ereader))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 24.4)
                (bin-exists-p "git")))
   :packages '(magit)
   :compile `(,(emacs-home* "config/setup-magit.el")))
  (list
   :cond (lambda ()
           (and (version-supported-p '<= 23.2)
                (bin-exists-p "racket")))
   :packages '(geiser))
  (list
   :cond (lambda ()
           (or (bin-exists-p "sbcl")))
   :packages '(slime)
   :compile `(,(emacs-home* "config/setup-slime.el"))))


