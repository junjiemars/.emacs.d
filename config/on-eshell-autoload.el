;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; eshells.el
;;;;


(defun on-eshell-init! ()
  "On \\=`eshell-mode\\=' initialization."
  (eval-when-compile (require 'em-term))
  (require 'em-term)
  (let ((eshell (*self-env-spec* :get :eshell)))
    (when (self-spec-> eshell :allowed)
      (let ((visuals (self-spec-> eshell :visual-commands)))
        (dolist* (x visuals)
          (append! x eshell-visual-commands t)))
      (setq% eshell-destroy-buffer-when-process-dies
             (self-spec-> eshell :destroy-buffer-when-process-dies))
      (setq% eshell-visual-subcommands
             (self-spec-> eshell :visual-subcommands))
      (setq% eshell-visual-options
             (self-spec-> eshell :visual-options))))
  ;; abbreviated `eshell' prompt
  (when-version% > 23
    (setq% eshell-save-history-on-exit t 'em-hist)
    (when% (and (require 'em-prompt)
                (require 'em-dirs))
      (setq eshell-prompt-function
            #'(lambda ()
                (concat (abbreviate-file-name (eshell/pwd))
                        (if (= (user-uid) 0) " # " " $ ")))))))

;;; `eshell' after load
(with-eval-after-load* 'eshell #'on-eshell-init!)


;; end of on-eshell-autoload.el
