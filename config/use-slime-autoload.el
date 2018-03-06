;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime-autoload
;;;;



(defadvice slime (before slime-before compile)
  ;; (eval-when-compile (require 'use-slime))
  (setq% slime-lisp-implementations
	 (common-lisp-implementations) use-slime)
  (add-hook 'splime-repl-mode-hook #'use-slime-repl-mode!)
  (slime-setup '(slime-fancy slime-asdf)))


(defun use-slime-repl-mode! ()
  "Hook into `slime-repl-mode-hook'"
  (safe-fn-when slime-selector 
    (global-set-key (kbd "C-c s s") 'slime-selector)))


;; (autoload 'use-slime-repl-mode!
;;   (v-home* "config/" "use-slime.elc"))


