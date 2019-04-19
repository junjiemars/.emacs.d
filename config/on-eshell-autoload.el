;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eshells.el
;;;;


(defmacro self-eshell-mode! ()
  "Set the basics of `eshell-mode'"
  `(progn
     (eval-when-compile (require 'em-term))
     (require 'em-term)
     (when (self-spec->*env-spec :eshell :allowed)
       (mapc (lambda (x)
               (add-to-list 'eshell-visual-commands x t #'string=))
             (self-spec->*env-spec :eshell :visual-commands))
       (setq% eshell-destroy-buffer-when-process-dies
              (self-spec->*env-spec
                :eshell :destroy-buffer-when-process-dies))
       (setq% eshell-visual-subcommands
              (self-spec->*env-spec :eshell :visual-subcommands))
       (setq% eshell-visual-options
              (self-spec->*env-spec :eshell :visual-options)))))


(with-eval-after-load 'eshell
  (self-eshell-mode!)

  ;; abbreviated `eshell' prompt
  (version-supported-when > 23
    (setq% eshell-save-history-on-exit t 'em-hist)
    (when% (and (require 'em-prompt)
                (require 'em-dirs))
      (setq eshell-prompt-function
            #'(lambda ()
                (concat (abbreviate-file-name (eshell/pwd))
                        (if (= (user-uid) 0) " # " " $ ")))))))


 ;; end of file
