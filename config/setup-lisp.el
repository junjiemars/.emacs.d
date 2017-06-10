;;;;
;; Lisp
;;;;


(defun set-lisp-basic-mode! ()
  ;; structured editing of s-expression data
  (enable-paredit-mode)
  ;; enable automatically adjust the identation of code
  (aggressive-indent-mode)
  ;; hilighting parentheses,brackets,and braces in minor mode
  (rainbow-delimiters-mode))

;; basic lisp mode 
(dolist (hook '(emacs-lisp-mode-hook
                ielm-mode-hook
                scheme-mode-hook
                lisp-mode-hook
                lisp-interaction-mode-hook))
  (add-hook hook #'set-lisp-basic-mode!))


;; Enable paredit in minibuffer on gnu/linux platform
(platform-supported-when
 gnu/linux
 (add-hook 'minibuffer-setup-hook
           #'enable-paredit-mode t))


;; Enable paredit in minbuffer on windows/darwin platform
(platform-supported-unless
    gnu/linux
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'enable-paredit-mode t))


;; On Windows C-) is not work
;; fix inconsistent `C-)' `C-c )' behavior:#9
(global-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)


;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
(terminal-supported-p
  (global-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
  (global-set-key (kbd "C-c }") 'paredit-forward-barf-sexp)
  (global-set-key (kbd "C-c {") 'paredit-backward-barf-sexp))

