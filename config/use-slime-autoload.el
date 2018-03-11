;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime-autoload
;;;;


(defadvice slime (before slime-before compile)
	;; "use `M-- M-x slime', \\{slime-repl-mode-map}"
	(require 'use-slime (v-home% "config/" "use-slime.elc"))
	(set-slime-lisp-implementations!)
  (add-hook 'slime-repl-mode-hook #'use-slime-repl-mode!)
  (slime-setup '(slime-fancy slime-asdf)))

(defadvice slime (after slime-after compile)
  (add-hook 'slime-repl-mode-hook #'use-slime-repl-mode!))


(defun use-slime-repl-mode! ()
  "Hook into `slime-repl-mode-hook'"
  (when-fn% slime-selector slime
    (global-set-key (kbd "C-c s s") 'slime-selector)))



