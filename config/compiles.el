;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; compiles.el
;;;;

;;; require

(require 'compile)
(require 'ansi-color)

;; end of require

(when-platform% windows-nt
  ;; There are no builtin `grep' in Windows, GNU's `grep' may use
  ;; the UNIX path in Windows which cannot be recognized by Emacs.
  ;; When such case occurred, we try to translate UNIX path to POSIX path.
  (defun compilation-find-file* (marker filename directory &rest formats)
    (let ((filename (if (string-match "^/\\([a-zA-Z]\\)/" filename)
                        (replace-match (concat (match-string 1 filename) ":/")
                                       t t filename)
                      filename)))
      (funcall (symbol-function '_compilation-find-file_)
               marker filename directory formats))))

(defun compile*-colorize-buffer! ()
  "Colorize compilation buffer."
  (when-fn% ansi-color-apply-on-region ansi-color
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))))

(when-platform% darwin
  ;; `next-error' cannot find the source file on Darwin.
  (defun compile*-make-change-dir (&optional buffer how)
    "Add the argument of make's -C option to \\=`compilation-search-path\\='."
    (ignore* buffer how)
    (when (eq major-mode 'compilation-mode)
      (let ((d (string-match*
                ".*make.*-C[ \t]*\\([-~._/a-zA-Z0-9]+\\)\\(?:\\S*\\|.*\\)$"
                compile-command
                1)))
        (when (and d (file-exists-p d))
          (push! d compilation-search-path t))))))

(defmacro compile*-command-name (command)
  "Return classified compile COMMAND name."
  `(string-match* "/?\\([^ /]+\\) +" ,command 1))

(defmacro compile*-buffer-name (command)
  "Make compilation buffer name with COMMAND."
  `(if ,command
       (format "*compilation-%s*" ,command)
     "*compilation*"))

(defun compile*-buffer-name-fn (command-or-mode)
  "Classify compilation buffer name with COMMAND-OR-MODE."
  (let ((c (cond ((string-equal "compilation" command-or-mode)
                  (compile*-command-name compile-command))
                 (t command-or-mode))))
    (compile*-buffer-name c)))

(defun compile*-recompile (&optional _ __)
  "Re-compile."
  (interactive)
  (cond ((null current-prefix-arg) (call-interactively #'recompile))
        (t (let ((current-prefix-arg nil))
             (call-interactively #'compile)))))

(defun compile*-regrep (&optional _)
  "Re-grep."
  (interactive)
  (cond (current-prefix-arg
         (cond ((save-excursion
                  (goto-char (point-min))
                  (forward-line 3)
                  (re-search-forward "^find" nil t 1))
                (call-interactively #'grep-find))
               (t (call-interactively #'grep))))
        (t (call-interactively #'recompile))))

(defun on-compile-init! ()
  "On \\=`compile\\=' initialization."
  (setq% compilation-buffer-name-function #'compile*-buffer-name-fn compile)
  (when-platform% windows-nt
    (defadvice* '_compilation-find-file_
      'compilation-find-file #'compilation-find-file*))
  (when-platform% darwin
    ;; `next-error' find source file
    (add-hook 'compilation-finish-functions
              #'compile*-make-change-dir
              (emacs-arch%)))
  (add-hook 'compilation-filter-hook #'compile*-colorize-buffer!)
  (when-var% compilation-mode-map compile
    ;; define `recompile' and `quit-window' key bindings
    (define-key compilation-mode-map "g" #'compile*-recompile)
    (define-key% compilation-mode-map "q" #'quit-window)
    (define-key% compilation-mode-map "n" #'next-error-no-select)
    (define-key% compilation-mode-map "p" #'previous-error-no-select))
  (setq% compilation-scroll-output t compile))


(defun on-grep-init! ()
  "On \\=`grep\\=' initialization."
  ;; define `recompile' and `quit-window' key binding for `grep'
  (when-var% grep-mode-map grep
    (define-key grep-mode-map "g" #'compile*-regrep)
    (define-key% grep-mode-map "q" #'quit-window)))


(defun on-make-mode-init! ()
  "On \\=`make-mode\\=' intialization."
  ;; spaced blackslash-region for makefile.
  (when-var% makefile-mode-map make-mode
    (when-fn% makefile-backslash-region make-mode
      (define-key
       makefile-mode-map (kbd% "C-c C-\\")
       (lambda (from to delete-flag)
         (interactive "r\nP")
         (fluid-let (indent-tabs-mode nil)
           (makefile-backslash-region from to delete-flag))))))
  (when-platform% darwin
    (when-fn% makefile-gmake-mode make-mode
      (when% (and (assoc-string "[Mm]akefile\\'" auto-mode-alist)
                  (executable-find*
                   "make"
                   (lambda (make)
                     (let ((x (shell-command* make "--version")))
                       (and (zerop (car x))
                            (string-match "^GNU Make.*" (cdr x)))))))
        (when% (assoc-string "[Mm]akefile\\'" auto-mode-alist)
          (setcdr (assoc-string "[Mm]akefile\\'" auto-mode-alist)
                  'makefile-gmake-mode))))))



(provide 'compiles)

;; end of compiles.el
