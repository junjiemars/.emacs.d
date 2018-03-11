;;;; -*- lexical-binding:t -*-
;;;;
;; autoloads
;;;;


(autoload 'system-cc-include
  (v-home% "config/" "cc.elc")
  "Returns a list of system include directories.")


(defun set-global-key! ()
  
  ;; open file or url at point
  (when-fn% find-file-at-point ffap
    (global-set-key (kbd "C-c b") 'find-file-at-point))

  (linum-mode-supported-p
    (global-set-key (kbd "C-c l") 'linum-mode))

  ;; Shows a list of buffers
  (global-set-key (kbd "C-x C-b") #'ibuffer)

  ;; interactive search key bindings.
  ;; by default, C-s runs isearch-forward, so this swaps the bindings.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; Interactive query replace key bindings.
  (global-set-key (kbd "M-%") 'query-replace-regexp)
  (global-set-key (kbd "C-M-%") 'query-replace-regexp)

  ;; toggle comment key strike
  (global-set-key (kbd "C-c ;") 'toggle-comment)

  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; but `C-x r g' can do thing by one hand
  (global-set-key (kbd "C-x r g") 'string-insert-rectangle)

  ;; Key binding to use "hippie expand" for text autocompletion
  ;; http://www.emacswiki.org/emacs/HippieExpand
  (global-set-key (kbd "M-/") 'hippie-expand)

  (package-supported-p

    ;; `bing-dict'
    (global-set-key (kbd "C-c d") 'bing-dict-brief)

    ;; `paredit'
    ;; On Windows C-) is not work
    ;; fix inconsistent `C-)' `C-c )' behavior:#9
    (global-set-key (kbd "C-c )") 'paredit-forward-slurp-sexp)

    ;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
    (terminal-supported-p
      (global-set-key (kbd "C-c (") 'paredit-backward-slurp-sexp)
      (global-set-key (kbd "C-c }") 'paredit-forward-barf-sexp)
      (global-set-key (kbd "C-c {") 'paredit-backward-barf-sexp)))

  )


(defun set-flavor-mode! ()
  
  ;; Changes all yes/no questions to y/n type
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Highlights matching parenthesis
  (show-paren-mode 1)

  ;; ido-mode allows you to more easily navigate choices. For example,
  ;; when you want to switch buffers, ido presents you with a list
  ;; of buffers in the the mini-buffer. As you start to type a buffer's
  ;; name, ido will narrow down the list of buffers to match the text
  ;; you've typed in
  ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
  (ido-mode t)

  ;; enable save minibuffer history
  (version-supported-if
      <= 24
      (savehist-mode)
    (savehist-mode t))

  (eww-mode-supported-p
    (set-default-browser!))

  ;; ielm basic
  (add-hook 'ielm-mode-hook #'eldoc-mode)

  ;; disable auto-save mode
  (setq auto-save-default nil)

  ;; enable save-place
  (version-supported-if
      <= 25.1
      (save-place-mode t)
    (setq% save-place t saveplace))

  
  ;; emacs lisp basic 
  (add-hook 'emacs-lisp-mode-hook #'set-emacs-lisp-mode!)

  ;; Shows all options when running apropos. For more info,
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
  ;;enable apropos-do-all, but slower
  (setq% apropos-do-all t apropos)

  ;; on Drawin: ls does not support --dired;
  ;; see `dired-use-ls-dired' for more defails
  (platform-supported-when
      darwin
    (with-eval-after-load 'dired
      (setq% ls-lisp-use-insert-directory-program nil)))


  ;; Makes killing/yanking interact with the clipboard

  ;; Save clipboard strings into kill ring before replacing them.
  ;; When one selects something in another program to paste it into Emacs,
  ;; but kills something in Emacs before actually pasting it,
  ;; this selection is gone unless this variable is non-nil
  ;; I'm actually not sure what this does but it's recommended?
  ;; http://emacswiki.org/emacs/CopyAndPaste
  (version-supported-if
      <= 24.1
      (setq% select-enable-clipboard t)
    (setq% x-select-enable-clipboard t))
  (version-supported-if
      <= 25.1
      (setq% select-enable-primary t select)
    (setq% x-select-enable-primary t select))

  ;; Save before kill
  (setq% save-interprogram-paste-before-kill t simple)

  ;; Mouse yank commands yank at point instead of at click.
  (setq% mouse-yank-at-point t mouse)

  ;; Lisp-friendly hippie expand
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol))


  ;; no need for ~ files when editing
  (setq% create-lockfiles nil)

  ;; don't use hard tabs
  (setq indent-tabs-mode nil)

  ;; disable electric indent mode
  (setq% electric-indent-mode nil electric)

  ;; enable column number mode
  (setq% column-number-mode t simple)

  ;; default tab-width
  (setq-default tab-width 2)

  ;; Greek letters C-x 8 <RET> greek small letter lambda
  ;; (global-set-key (kbd "C-c l") "λ")

  ;; enable upcase/downcase region
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)

  (add-hook 'sh-mode-hook #'set-sh-mode!)

  (package-supported-p
    ;; basic lisp mode 
    (dolist (hook '(emacs-lisp-mode-hook
										ielm-mode-hook
										scheme-mode-hook
										lisp-mode-hook
										lisp-interaction-mode-hook))
      (add-hook hook #'set-lisp-basic-mode!))

    ;; enable paredit in minibuffer on gnu/linux platform
    (platform-supported-if
				gnu/linux
				(add-hook 'minibuffer-setup-hook
									#'enable-paredit-mode t)
      ;; enable paredit in minbuffer on windows/darwin platform
      (add-hook 'eval-expression-minibuffer-setup-hook
								#'enable-paredit-mode t)))
   ;; end of package: paredit

  ;; Terminal
  (terminal-supported-p
    (linum-mode-supported-p
      ;; line number format on Terminal
      (setq% linum-format "%2d " linum)))

   ;; end of Terminal

  
  )


(add-hook 'after-init-hook #'set-flavor-mode! t)
(add-hook 'after-init-hook #'set-global-key! t)

