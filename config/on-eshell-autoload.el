;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eshells.el
;;;;


(defmacro self-eshell-mode! ()
  "Set the basics of `eshell-mode'"
  `(progn
     (eval-when-compile (require 'em-term))
     (require 'em-term)
     (when (*self-env-spec* :get :eshell :allowed)
       (let ((visuals (*self-env-spec* :get :eshell :visual-commands)))
         (dolist* (x visuals)
           (push! x eshell-visual-commands t t)))
       (setq% eshell-destroy-buffer-when-process-dies
              (*self-env-spec* :get
                               :eshell :destroy-buffer-when-process-dies))
       (setq% eshell-visual-subcommands
              (*self-env-spec* :get :eshell :visual-subcommands))
       (setq% eshell-visual-options
              (*self-env-spec* :get :eshell :visual-options)))))


(with-eval-after-load 'eshell
  (self-eshell-mode!)

  ;; abbreviated `eshell' prompt
  (when-version% > 23
    (setq% eshell-save-history-on-exit t 'em-hist)
    (when% (and (require 'em-prompt)
                (require 'em-dirs))
      (setq eshell-prompt-function
            #'(lambda ()
                (concat (abbreviate-file-name (eshell/pwd))
                        (if (= (user-uid) 0) " # " " $ ")))))))


 ;; end of file
