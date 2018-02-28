;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime-autoload
;;;;



(defadvice slime (before slime-before compile)
  (eval-when-compile (require 'use-slime))
  (set-default 'slime-lisp-implementations (common-lisp-implementations))
  (add-hook 'slime-repl-mode-hook #'use-slime-repl-mode!)
  (slime-setup '(slime-fancy slime-asdf)))


(autoload 'use-slime-repl-mode!
  (v-home* "config/" "use-slime.elc")
  "Using slime functions, `autoload'")
