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

(defmacro when-feature-paredit% (&rest body)
  `(if-feature-paredit%
       (progn% ,@body)))

;; end of macro


(defun lisp*-featured! ()
  "Featured \\=`lisp-mode\\='."
  (unless (or (string= "*scratch*" (buffer-name))
              (string= "*ielm*" (buffer-name)))
    ;; don't autoload: `aggressive' indent
    ;; (if-feature-aggressive-indent% (make-thread* #'aggressive-indent-mode))
    ;; `paredit' structured editing of s-expression data
    (if-feature-paredit% (make-thread* #'enable-paredit-mode))
    ;; `rainbow-delimiters': hilighting parentheses, brackets
    (if-feature-rainbow-delimiters% (make-thread* #'rainbow-delimiters-mode))))



(defun on-use-emacs-lisp-init! ()
  "On \\=`elisp-mode\\=' initialization."
  (append! #'lisp*-featured! emacs-lisp-mode-hook))

(defun on-use-lisp-init! ()
  "On \\=`lisp-mode\\=' initialization."
  (append! #'lisp*-featured! lisp-mode-hook t))


;;; `elisp-mode' after load
(eval-after-load 'elisp-mode #'on-use-emacs-lisp-init!)

(eval-after-load 'lisp-mode #'on-use-lisp-init!)


(defun on-use-scheme-init! ()
  "On \\=`scheme\\=' initialization."
  ;; disable auto active other scheme hooks
  (when-var% scheme-mode-hook 'scheme
    (setq scheme-mode-hook nil))
  (append! #'lisp*-featured! scheme-mode-hook))


;;; `scheme' after load
(eval-after-load 'scheme #'on-use-scheme-init!)


;;; `paredit'

(when-feature-paredit%
 (defun paredit*-refine-keys! ()
   "Refine the keys of \\=`paredit\\='."
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
 (defun toggle-paredit! ()
   "Toggle \\=`paredit\\='."
   (interactive)
   (let ((m (paredit-mode (if paredit-mode -1 +1))))
     (when m (paredit*-refine-keys!))
     (message "paredit %s" (if m "on" "off")))))


;;; `paredit' after load
(when-feature-paredit%
 (eval-after-load 'paredit #'paredit*-refine-keys!))

;; end of `paredit'



;; end of use-lisp-autoload.el
