;;;; -*- lexical-binding:t -*-
;;;;
;; use-cider-autoload
;;;;


(defun use-cider-repl-mode! ()
  "Hook into `cider-repl-mode-hook'"
  (eldoc-mode)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


(defadvice cider-jack-in (before cider-jack-in-before compile)
  ;; minor modes for cider
  (add-hook 'cider-repl-mode-hook #'use-cider-repl-mode!))


(autoload 'use-cider-repl-mode!
  (v-home% "config/" "use-cider.elc"))
