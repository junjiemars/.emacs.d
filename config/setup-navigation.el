;;;;
;; Navigation
;;;;


;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
;; But it's loading slow, so load it after-init


(defun self-ido-ubiquitous! ()
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  
  (ido-ubiquitous-mode 1)

  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  ;; https://github.com/nonsequitur/smex
  
  (global-set-key (kbd "M-x")
                  (lambda ()
                    (interactive)
                    (or (boundp 'smex-cache)
                        (smex-initialize))
                    (smex)))
  
  (safe-setq smex-save-file (v-home! ".smex/" ".smex-items")))


(add-hook 'after-init-hook #'self-ido-ubiquitous!)





