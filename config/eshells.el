;;;; -*- lexical-binding:t -*-
;;;;
;; eshells
;;;;


(version-supported-when > 23
  (setq% eshell-save-history-on-exit t em-hist))


(defun eshell-mode! ()
  (eval-when-compile (require 'em-term))
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


(defadvice eshell (after eshell-after compile)
  (eshell-mode!))


(provide 'eshells)
