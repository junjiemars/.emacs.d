;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-cider-autoload.el
;;;;


(defun use-cider-repl-mode! ()
  "Hook into `cider-repl-mode-hook'"
  (eldoc-mode)
  (enable-paredit-mode)
  (rainbow-delimiters-mode)
  (aggressive-indent-mode))


(autoload 'use-cider-repl-mode!
  (v-home% "config/" "use-cider.elc"))


(with-eval-after-load 'cider
	(add-hook 'cider-repl-mode-hook #'use-cider-repl-mode!))
