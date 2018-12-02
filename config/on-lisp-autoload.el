;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
;;;;


(with-eval-after-load 'lisp-mode
  (safe-local-variable* 'Syntax)
  (safe-local-variable* 'Base)
  (safe-local-variable* 'Package))


(defun set-basic-lisp-mode! ()
  (setq indent-tabs-mode nil))


(version-supported-if
    <= 25.0
    (with-eval-after-load 'elisp-mode
      (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))
  (with-eval-after-load 'lisp-mode
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))


(package-supported-p
  
  (defun set-featured-lisp-mode! ()
    "Set Lisp basic minor modes."
    (cond ((or (string= "*scratch*" (buffer-name))
               (string= "*ielm*" (buffer-name))))
          (t (feature-allowed-p paredit
               ;; structured editing of s-expression data
               (enable-paredit-mode))

             (feature-allowed-p rainbow-delimiters
               ;; hilighting parentheses,brackets,and braces in minor mode
               (rainbow-delimiters-mode))

             (feature-allowed-p aggressive-indent
               ;; aggressive indent
               (aggressive-indent-mode))))))


(package-supported-p
  (feature-paredit-supported-p
    
    (defun enable-paredit-mode-in-minibuffer! ()
      (platform-supported-if 'gnu/linux
          (when (eq 'eval-expression this-command)
            (enable-paredit-mode))
        (enable-paredit-mode)))))


(with-eval-after-load 'lisp-mode
  (add-hook 'lisp-mode-hook #'set-basic-lisp-mode!)
  (add-hook 'emacs-lisp-mode-hook #'set-basic-lisp-mode!)
  (package-supported-p
    (add-hook 'lisp-mode-hook #'set-featured-lisp-mode!)
    (add-hook 'emacs-lisp-mode-hook #'set-featured-lisp-mode!)))


(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook #'set-basic-lisp-mode!)    
  (package-supported-p
    (add-hook 'scheme-mode-hook #'set-featured-lisp-mode!)))


(package-supported-p
  (feature-allowed-p paredit

    (platform-supported-if
        ;; enable `paredit' in `minibuffer'
        'gnu/linux
        (add-hook 'minibuffer-setup-hook
                  #'enable-paredit-mode-in-minibuffer! t)
      (add-hook 'eval-expression-minibuffer-setup-hook
                #'enable-paredit-mode-in-minibuffer! t))

    (with-eval-after-load 'paredit    
      ;; define `paredit' keymap
      ;; On Windows C-) is not work
      ;; fix inconsistent `C-)' `C-c )' behavior:#9
      ;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
      (when-var% paredit-mode-map paredit
        (define-key% paredit-mode-map (kbd "C-c )") #'paredit-forward-slurp-sexp)
        (define-key% paredit-mode-map (kbd "C-c (") #'paredit-backward-slurp-sexp)
        (define-key% paredit-mode-map (kbd "C-c }") #'paredit-forward-barf-sexp)
        (define-key% paredit-mode-map (kbd "C-c {") #'paredit-backward-barf-sexp)))))


 ;; end of feature: paredit


(version-supported-when > 24 
  ;; fix: no TAB completion on ancient Emacs
  (defun define-eval-or-execute-key ()
    (cond ((eq 'eval-expression this-command)
           (define-key (current-local-map) (kbd "TAB") #'lisp-complete-symbol))
          ((eq 'execute-extended-command this-command)
           (define-key (current-local-map) (kbd "TAB") #'minibuffer-complete))))

  (add-hook 'minibuffer-setup-hook #'define-eval-or-execute-key t))


(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'set-basic-lisp-mode!)
  (add-hook 'ielm-mode-hook #'eldoc-mode))


