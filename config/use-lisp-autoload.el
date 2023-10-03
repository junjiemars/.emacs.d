;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lisp-autoload.el
;;;;


(defmacro-if-feature% aggressive-indent)
(defmacro-if-feature% paredit)
(defmacro-if-feature% rainbow-delimiters)


(defun set-featured-lisp-mode! ()
  "Set Lisp basic minor modes."
  (cond ((or (string= "*scratch*" (buffer-name))
             (string= "*ielm*" (buffer-name))))
        (t
         ;; structured editing of s-expression data
         (if-feature-paredit% (enable-paredit-mode))

         ;; hilighting parentheses,brackets,and braces in minor mode
         (if-feature-rainbow-delimiters% (rainbow-delimiters-mode))

         ;; aggressive indent
         (if-feature-aggressive-indent% (aggressive-indent-mode)))))


;;; `lisp-mode'
(if-version%
    <= 25.0

    ;; `lisp-mode'
    (with-eval-after-load 'elisp-mode
      (add-hook 'lisp-mode-hook #'set-featured-lisp-mode!)
      (add-hook 'emacs-lisp-mode-hook #'set-featured-lisp-mode!))

  ;; `elisp-mode'
  (with-eval-after-load 'lisp-mode
    (add-hook 'lisp-mode-hook #'set-featured-lisp-mode!)
    (add-hook 'emacs-lisp-mode-hook #'set-featured-lisp-mode!)))


;;; `scheme'
(with-eval-after-load 'scheme

  ;; disable auto active other scheme hooks
  (when-var% scheme-mode-hook 'scheme
    (setq scheme-mode-hook nil))

  (add-hook 'scheme-mode-hook #'set-featured-lisp-mode!))


(if-feature-paredit%

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
      (define-key% paredit-mode-map (kbd "C-c C-q")
        #'paredit-reindent-defun)

      (define-key% paredit-mode-map (kbd "C-c -") #'paredit-splice-sexp)
      (define-key% paredit-mode-map (kbd "C-j") #'paredit-newline)
      (define-key% paredit-mode-map (kbd "RET") #'newline*)

      ;; default `paredit-convolute-sexp' key `M-?' conflicts with
      ;; `xref-find-references'
      (define-key% paredit-mode-map (kbd "C-c ?") #'paredit-convolute-sexp)
      (define-key% paredit-mode-map (kbd "M-?") #'xref-find-references)


      ;; disable `paredit-splice-sexp' key, `M-s' conflicts with
      ;; `isearch' command prefix.
      (define-key% paredit-mode-map (kbd "M-s") nil)
      ;; disable `paredit-up/down' key
      (define-key% paredit-mode-map (kbd "C-M-f") nil)
      (define-key% paredit-mode-map (kbd "C-M-b") nil)
      (define-key% paredit-mode-map (kbd "C-M-d") nil)
      (define-key% paredit-mode-map (kbd "C-M-u") nil)
      ;; disable `paredit-forward-up' key
      (define-key% paredit-mode-map (kbd "C-M-n") nil)
      ;; disable `paredit-forward-down' key
      (define-key% paredit-mode-map (kbd "C-M-p") nil)
      ;; disable `paredit-kill' key
      (define-key% paredit-mode-map (kbd "C-k") nil)
      ;; disable `paredit-reindent-defun' key `M-q' conflicts with
      ;; `fill-paragraph'.
      (define-key% paredit-mode-map (kbd "M-q") nil))))


;; end of `paredit'



;; end of use-lisp-autoload.el
