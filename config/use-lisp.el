;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lisp.el
;;;;

;;; macro

(defmacro-if-feature% aggressive-indent)
(defmacro-if-feature% paredit)
(defmacro-if-feature% rainbow-delimiters)

(defmacro when-feature-paredit% (&rest body)
  (declare (indent 0))
  (if-feature-paredit%
      `(progn% ,@body)
    `(comment ,@body)))

;; end of macro


(defun lisp*-featured! ()
  "Featured \\=`lisp-mode\\='."
  (unless (or (string= "*scratch*" (buffer-name))
              (string= "*ielm*" (buffer-name)))
    ;; don't autoload: `aggressive' indent
    ;; (if-feature-aggressive-indent% (make-thread* #'aggressive-indent-mode))
    ;; `paredit' structured editing of s-expression data
    (if-feature-paredit%
        (use-paredit-init!))
    ;; `rainbow-delimiters': hilighting parentheses, brackets
    (rainbow-delimiters-mode)))


(defun use-emacs-lisp-init! ()
  "On \\=`elisp-mode\\=' initialization."
  (append! #'lisp*-featured! emacs-lisp-mode-hook))

(defun use-lisp-init! ()
  "On \\=`lisp-mode\\=' initialization."
  (append! #'lisp*-featured! lisp-mode-hook))

(defun use-scheme-init! ()
  "On \\=`scheme\\=' initialization."
  ;; disable auto active other scheme hooks
  (setq% scheme-mode-hook nil 'scheme)
  (append! #'lisp*-featured! scheme-mode-hook))

;;; `paredit'

(when-feature-paredit%
  (defun paredit*-refine-keys! ()
    ;; On Windows C-) is not work
    ;; fix inconsistent `C-)' and `C-c )' behavior:#9
    ;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
    (define-key% paredit-mode-map (kbd "C-c )") #'paredit-forward-slurp-sexp)
    (define-key% paredit-mode-map (kbd "C-c (") #'paredit-backward-slurp-sexp)
    (define-key% paredit-mode-map (kbd "C-c }") #'paredit-forward-barf-sexp)
    (define-key% paredit-mode-map (kbd "C-c {") #'paredit-backward-barf-sexp)
    (define-key% paredit-mode-map (kbd "C-c C-q") #'paredit-reindent-defun)
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
    ;; ;; disable `paredit-delete-char' key
    ;; (define-key% paredit-mode-map (kbd "C-d") nil)
    ;; ;; disable `paredit-backward-delete' key
    ;; (define-key% paredit-mode-map (kbd "DEL") nil)
    ;; disable `paredit-forward-kill-word' key
    (define-key% paredit-mode-map (kbd "M-d") nil)
    ;; disable `paredit-kill' key
    (define-key% paredit-mode-map (kbd "C-k") nil)
    ;; disable `paredit-up/down' key
    (define-key% paredit-mode-map (kbd "C-M-f") nil)
    (define-key% paredit-mode-map (kbd "C-M-b") nil)
    (define-key% paredit-mode-map (kbd "C-M-d") nil)
    (define-key% paredit-mode-map (kbd "C-M-u") nil)
    ;; disable `paredit-forward-up' key
    (define-key% paredit-mode-map (kbd "C-M-n") nil)
    ;; disable `paredit-forward-down' key
    (define-key% paredit-mode-map (kbd "C-M-p") nil)
    ;; disable `paredit-reindent-defun' key `M-q' conflicts with
    ;; `fill-paragraph'.
    (define-key% paredit-mode-map (kbd "M-q") nil)))


(when-feature-paredit%
  (defun use-paredit-init! ()
    (enable-paredit-mode)
    (paredit*-refine-keys!)))

(when-feature-paredit%
  (defun toggle-paredit! ()
    "Toggle \\=`paredit\\='."
    (interactive)
    (let ((m (paredit-mode (if paredit-mode -1 +1))))
      (when m (paredit*-refine-keys!))
      (message "paredit %s" (if m "on" "off")))))

;; ;;; `paredit' after load
;; (when-feature-paredit%
;;   (with-eval-after-load 'paredit
;;     (use-paredit-init!)))

;; end of `paredit'

(provide 'use-lisp)

;; end of use-lisp.el
