;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-edit-autoload.el
;;;;


(defmacro-if-feature% aggressive-indent)
(defmacro-if-feature% paredit)
(defmacro-if-feature% rainbow-delimiters)


(defun set-featured-lisp-mode! ()
  "Set Lisp basic minor modes."
  (cond ((or (string= "*scratch*" (buffer-name))
             (string= "*ielm*" (buffer-name))))
        (t (when-feature-allowed% if-feature-paredit%
             ;; structured editing of s-expression data
             (enable-paredit-mode))

           (when-feature-allowed% if-feature-rainbow-delimiters%
             ;; hilighting parentheses,brackets,and braces in minor mode
             (rainbow-delimiters-mode))

           (when-feature-allowed% if-feature-aggressive-indent%
             ;; aggressive indent
             (aggressive-indent-mode)))))


;;; `lisp-mode'
(with-eval-after-load 'lisp-mode

  (add-hook 'lisp-mode-hook #'set-featured-lisp-mode!))


;;; `elisp-mode'
(with-eval-after-load 'elisp-mode

  (add-hook 'emacs-lisp-mode-hook #'set-featured-lisp-mode!))


;;; `scheme'
(with-eval-after-load 'scheme

  ;; disable auto active other scheme hooks
  (when-var% scheme-mode-hook 'scheme
    (setq scheme-mode-hook nil))

  (add-hook 'scheme-mode-hook #'set-featured-lisp-mode!))


(when-feature-allowed% if-feature-paredit%

  (with-eval-after-load 'paredit

    ;; define `paredit' keymap
    ;; On Windows C-) is not work
    ;; fix inconsistent `C-)' and `C-c )' behavior:#9
    ;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
    (when-var% paredit-mode-map 'paredit

      (define-key% paredit-mode-map (kbd "C-c )")
        #'paredit-forward-slurp-sexp)
      (define-key% paredit-mode-map (kbd "C-c (")
        #'paredit-backward-slurp-sexp)
      (define-key% paredit-mode-map (kbd "C-c }")
        #'paredit-forward-barf-sexp)
      (define-key% paredit-mode-map (kbd "C-c {")
        #'paredit-backward-barf-sexp)

      (define-key% paredit-mode-map (kbd "C-c -") #'paredit-splice-sexp)
      (define-key% paredit-mode-map (kbd "C-j") #'paredit-newline)
      (define-key% paredit-mode-map (kbd "RET") #'newline*)

      ;; default `paredit-convolute-sexp' keybinding `M-?' conflicts with
      ;; `xref-find-references'
      (define-key% paredit-mode-map (kbd "C-c ?") #'paredit-convolute-sexp)
      (define-key% paredit-mode-map (kbd "M-?") #'xref-find-references)

      ;; default `paredit-splice-sexp' keybinding `M-s' conflicts with
      ;; `isearch' command prefix.
      (define-key% paredit-mode-map (kbd "M-s") nil)
      ;; disable `paredit-up/down' keybindings
      (define-key% paredit-mode-map (kbd "C-M-f") nil)
      (define-key% paredit-mode-map (kbd "C-M-b") nil)
      (define-key% paredit-mode-map (kbd "C-M-d") nil)
      (define-key% paredit-mode-map (kbd "C-M-u") nil)
      ;; disable `paredit-forward-up' keybindings
      (define-key% paredit-mode-map (kbd "C-M-n") nil)
      (define-key% paredit-mode-map (kbd "C-M-p") nil))))


 ;; end of feature: paredit



;; end of file
