;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; compiles.el
;;;;

(when-platform% 'windows-nt
  ;; There are no builtin `grep' in Windows, GNU's `grep' may use
  ;; the UNIX path in Windows which cannot be recognized by Emacs.
  ;; When such case occurred, we try to translate UNIX path to POSIX path.
  (defadvice compilation-find-file
      (before compilation-find-file-before first compile disable)
    (when (string-match "^/\\([a-zA-Z]\\)/" (ad-get-arg 1))
      (ad-set-arg
       1 ;; filename argument
       (replace-match (concat (match-string 1 (ad-get-arg 1)) ":/")
                      t t (ad-get-arg 1))))))


(defun compilation*-colorize-buffer! ()
  "Colorize compilation buffer."
  (when-fn% 'ansi-color-apply-on-region 'ansi-color
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (require 'ansi-color)
        (ansi-color-apply-on-region (point-min) (point-max))))))


(when-platform% 'darwin
  ;; `next-error' cannot find the source file on Darwin.
  (defun compilation*-make-change-dir (&optional buffer how)
    "Add the argument of make's -C option to \\=`compilation-search-path\\='."
    (ignore* buffer how)
    (when (eq major-mode 'compilation-mode)
      (let ((d (string-match*
                ".*make.*-C[ \t]*\\([-~._/a-zA-Z0-9]+\\)\\(?:\\S*\\|.*\\)$"
                compile-command
                1)))
        (when (and d (file-exists-p d))
          (push! d compilation-search-path t))))))

(defun compilation*-compile-command (command)
  "Return classified compile command."
  (string-match* " *\\([-a-zA-z0-9]+\\) *" command 1))

(defun compilation*-buffer-name (command-or-mode)
  "Classify compilation buffer name based on COMMAND-OR-MODE."
  (format "*compilation-%s*"
          (compilation*-compile-command
           (cond ((string= "compilation" command-or-mode)
                  compile-command)
                 (t command-or-mode)))))

(defun compilation*-recompile (&optional _ __)
  "Re-compile."
  (interactive)
  (when-version% > 27
    (eval-when-compile (require 'compile))
    (declare-function recompile 'compile))
  (cond ((and current-prefix-arg
              (not
               (string=
                (compilation*-compile-command (car compilation-arguments))
                (compilation*-compile-command compile-command))))
         (let ((current-prefix-arg nil))
           (call-interactively #'compile)))
        (t (let ((compile-command (or (car compilation-arguments)
                                      compile-command)))
             (call-interactively #'recompile)))))

(defun on-compile-init! ()
  "On \\=`compile\\=' initialization."
  (inhibit-gc
    (setq% compilation-buffer-name-function #'compilation*-buffer-name 'compile)
    (when-platform% 'windows-nt
      ;; compile and activate `compilation-find-file' advice on Windows
      (ad-enable-advice #'compilation-find-file 'before
                        "compilation-find-file-before")
      (ad-activate #'compilation-find-file t))
    (when-platform% 'darwin
      ;; `next-error' find source file
      (add-hook 'compilation-finish-functions
                #'compilation*-make-change-dir
                (emacs-arch)))
    (add-hook 'compilation-filter-hook #'compilation*-colorize-buffer!)
    (when-var% compilation-mode-map 'compile
      ;; define `recompile' and `quit-window' key bindings
      (define-key% compilation-mode-map (kbd "g") #'compilation*-recompile)
      (define-key% compilation-mode-map (kbd "q") #'quit-window))
    (setq% compilation-scroll-output t 'compile)))


(defun on-grep-init! ()
  "On \\=`grep\\=' initialization."
  (inhibit-gc
    ;; define `recompile' and `quit-window' key binding for `grep'
    (when-var% grep-mode-map 'grep
      (define-key% grep-mode-map (kbd "g") #'compilation*-recompile)
      (define-key% grep-mode-map (kbd "q") #'quit-window))))


(defun on-make-mode-init! ()
  "On \\=`make-mode\\=' intialization."
  (inhibit-gc
    ;; spaced blackslash-region for makefile.
    (when-var% makefile-mode-map 'make-mode
      (when-fn% 'makefile-backslash-region 'make-mode
        (define-key%
         makefile-mode-map (kbd "C-c C-\\")
         #'(lambda (from to delete-flag)
             (interactive "r\nP")
             (fluid-let (indent-tabs-mode nil)
               (makefile-backslash-region from to delete-flag))))))
    (when-platform% 'darwin
      (when-fn% 'makefile-gmake-mode 'make-mode
        (when% (and (assoc-string "[Mm]akefile\\'" auto-mode-alist)
                    (executable-find%
                     "make"
                     (lambda (make)
                       (let ((x (shell-command* make "--version")))
                         (and (zerop (car x))
                              (string-match "^GNU Make.*" (cdr x)))))))
          (when% (assoc-string "[Mm]akefile\\'" auto-mode-alist)
            (setcdr (assoc-string "[Mm]akefile\\'" auto-mode-alist)
                    'makefile-gmake-mode)))))))



(provide 'compiles)

;; end of compiles.el
