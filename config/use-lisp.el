;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-lisp.el
;;;;

;;; require

(require% 'ed (v-home%> "config/ed"))

;; end of require

(defun lisp*-featured! ()
  "Featured \\=`lisp-mode\\='."
  (unless (or (string= "*scratch*" (buffer-name))
              (string= "*ielm*" (buffer-name)))
    ;; don't autoload: `aggressive' indent
    ;; (if-feature-aggressive-indent% (make-thread* #'aggressive-indent-mode))
    ;; `paredit' structured editing of s-expression data
    (when-feature% paredit
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

(when-feature% paredit
  (defun paredit*-refine-keys! ()
    ;; On Windows C-) is not work
    ;; fix inconsistent `C-)' and `C-c )' behavior:#9
    ;; On Terminal mode, Ctrl+Shift combination can't send to Emacs
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd% "C-d") #'paredit-delete-char)
      (define-key map (kbd% "DEL") #'paredit-backward-delete)
      (define-key map (kbd% "<deletechar>") #'paredit-forward-delete)
      (define-key map (kbd% "[") #'paredit-open-square)
      (define-key map (kbd% "]") #'paredit-close-square)
      (define-key map (kbd% "\"") #'paredit-doublequote)
      (define-key map (kbd% "(") #'paredit-open-round)
      (define-key map (kbd% ")") #'paredit-close-round)
      (define-key map (kbd% "C-c )") #'paredit-forward-slurp-sexp)
      (define-key map (kbd% "C-c (") #'paredit-backward-slurp-sexp)
      (define-key map (kbd% "C-c }") #'paredit-forward-barf-sexp)
      (define-key map (kbd% "C-c {") #'paredit-backward-barf-sexp)
      (define-key map (kbd% "C-c C-q") #'paredit-reindent-defun)
      (define-key map (kbd% "C-c -") #'paredit-splice-sexp)
      (define-key map (kbd% "C-j") #'paredit-newline)
      (define-key map (kbd% "RET") #'newline*)
      (setcdr (assoc 'paredit-mode minor-mode-map-alist) map))))

(when-feature% paredit
  (defun use-paredit-init! ()
    (enable-paredit-mode)
    (paredit*-refine-keys!)))

(when-feature% paredit
  (defun toggle-paredit! ()
    "Toggle \\=`paredit\\='."
    (interactive)
    (let ((m (paredit-mode (if paredit-mode -1 +1))))
      (when m (paredit*-refine-keys!))
      (message "paredit %s" (if m "on" "off")))))

;; end of `paredit'

(provide 'use-lisp)

;; end of use-lisp.el
