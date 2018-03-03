;;;; -*- lexical-binding:t -*-
;;;;
;; eshells
;;;;


(version-supported-when > 23
  (setq-default eshell-save-history-on-exit t))


(defun eshell-mode! ()
  (eval-when-compile (require 'em-term))
  (self-safe-call
   "env-spec"
   (when (self-spec->* :eshell :allowed)
     (dolist (x (self-spec->* :eshell :visual-commands))
       (add-to-list 'eshell-visual-commands x t #'string=))
     (safe-setq eshell-destroy-buffer-when-process-dies
                (self-spec->* :eshell :destroy-buffer-when-process-dies))
     (safe-setq eshell-visual-subcommands
                (self-spec->* :eshell :visual-subcommands))
     (safe-setq eshell-visual-options
                (self-spec->* :eshell :visual-options)))))


(defadvice eshell (after eshell-after compile)
  (eshell-mode!))


(provide 'eshells)
