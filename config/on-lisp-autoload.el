;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-lisp-autoload.el
;;;;


(with-eval-after-load 'lisp-mode
  (safe-local-variable* 'Syntax)
  (safe-local-variable* 'Base)
  (safe-local-variable* 'Package))


(if-version%
    <= 25.0
    (with-eval-after-load 'elisp-mode
      (add-hook 'emacs-lisp-mode-hook #'eldoc-mode))
  (with-eval-after-load 'lisp-mode
    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)))


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


;; (if-feature-paredit%

;;     (defun enable-paredit-mode-in-minibuffer! ()
;;       (if-platform% 'gnu/linux
;;           (when (eq 'eval-expression this-command)
;;             (enable-paredit-mode))
;;         (enable-paredit-mode))))


(with-eval-after-load 'lisp-mode
  (when-package%
    (add-hook 'lisp-mode-hook #'set-featured-lisp-mode!)
    (add-hook 'emacs-lisp-mode-hook #'set-featured-lisp-mode!)))


(with-eval-after-load 'scheme
  ;; disable auto active other scheme hooks
  (when-var% scheme-mode-hook 'scheme
    (setq scheme-mode-hook nil))
  ;; featured
  (when-package%
    (add-hook 'scheme-mode-hook #'set-featured-lisp-mode!)))


(when-feature-allowed% if-feature-paredit%

  ;; (if-platform%
  ;;     ;; enable `paredit' in `minibuffer'
  ;;     'gnu/linux
  ;;     (add-hook 'minibuffer-setup-hook
  ;;               #'enable-paredit-mode-in-minibuffer! t)
  ;;   (add-hook 'eval-expression-minibuffer-setup-hook
  ;;             #'enable-paredit-mode-in-minibuffer! t))

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
      (define-key% paredit-mode-map (kbd "C-M-u") nil))))


 ;; end of feature: paredit


;; fix: no TAB completion in minibuffer on ancient Emacs.
(if-key% minibuffer-local-map
    (kbd "TAB")
    (lambda (def) (memq def '(self-insert-command)))
    (progn
      (defun minibuffer-tab-completion! ()
        "TAB as completion key in minibuffer."
        ;; `lisp-complete-symbol' is an obsolete since Emacs24.4
        (define-key minibuffer-local-map (kbd "TAB")
          (if-fn% 'completion-at-point 'minibuffer
                  (if-version% > 24
                               #'lisp-complete-symbol
                    #'completion-at-point)
            #'lisp-complete-symbol)))
      (add-hook 'minibuffer-setup-hook #'minibuffer-tab-completion! t)))


(defun set-ielm-mode! ()
  (when-lexical%
    (setq lexical-binding t)))

(with-eval-after-load 'ielm
  (add-hook 'ielm-mode-hook #'eldoc-mode t)
  (add-hook 'ielm-mode-hook #'set-ielm-mode!))


;; end of file
