;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eshells.el
;;;;


(version-supported-when > 23
  (setq% eshell-save-history-on-exit t em-hist))


(defun eshell-mode! ()
  (eval-when-compile (require 'em-term))
  (require 'em-term)
  (when (self-spec->*env-spec :eshell :allowed)
    (dolist (x (self-spec->*env-spec :eshell :visual-commands))
      (add-to-list 'eshell-visual-commands x t #'string=))
    (setq% eshell-destroy-buffer-when-process-dies
	   (self-spec->*env-spec
	     :eshell :destroy-buffer-when-process-dies))
    (setq% eshell-visual-subcommands
	   (self-spec->*env-spec :eshell :visual-subcommands))
    (setq% eshell-visual-options
	   (self-spec->*env-spec :eshell :visual-options))))


(with-eval-after-load 'eshell
  (eshell-mode!))

