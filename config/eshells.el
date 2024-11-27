;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eshells.el
;;;;


(defun eshell-spec->* (&optional key)
  "Extract :eshell from env-spec via KEY."
  (cond (key (*self-env-spec* :get :eshell key))
        (t (*self-env-spec* :get :eshell))))



(defun on-eshell-init! ()
  "On \\=`eshell-mode\\=' initialization."
  (eval-when-compile (require 'em-term))
  (require 'em-term)
  (when (eshell-spec->* :allowed)
    (dolist* (x (eshell-spec->* :visual-commands))
      (append! x eshell-visual-commands t))
    (setq% eshell-destroy-buffer-when-process-dies
           (eshell-spec->* :destroy-buffer-when-process-dies))
    (setq% eshell-visual-subcommands
           (eshell-spec->* :visual-subcommands))
    (setq% eshell-visual-options
           (eshell-spec->* :visual-options)))
  ;; abbreviated `eshell' prompt
  (when-version% > 23
    (setq% eshell-save-history-on-exit t 'em-hist)
    (when% (and (require 'em-prompt)
                (require 'em-dirs))
      (setq eshell-prompt-function
            #'(lambda ()
                (concat (abbreviate-file-name (eshell/pwd))
                        (if (= (user-uid) 0) " # " " $ ")))))))


(provide 'eshells)

;; end of eshells.el
