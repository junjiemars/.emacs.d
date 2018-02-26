;;;; -*- lexical-binding:t -*-
;;;;
;; eshells
;;;;


(version-supported-when > 23
  (setq-default eshell-save-history-on-exit t))


(defun set-eshell-mode! ()
  (eval-when-compile (require 'em-term))
  (self-safe-call*
   "env-spec"
   (dolist (x (self-spec->* :eshell :visual-commands))
     (add-to-list 'eshell-visual-commands x t #'string=))
   (safe-setq eshell-destroy-buffer-when-process-dies
              (self-spec->* :eshell :destroy-buffer-when-process-dies))
   (safe-setq eshell-visual-subcommands
              (self-spec->* :eshell :visual-subcommands))
   (safe-setq eshell-visual-options
              (self-spec->* :eshell :visual-options))))


(self-safe-call*
 "env-spec"
 (when (self-spec->* :eshell :allowed)
   
   (defadvice eshell (after eshell-after compile)
     (set-eshell-mode!))))


(provide 'eshells)
