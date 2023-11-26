;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-compile-autoload.el
;;;;


(when-platform% 'windows-nt
  ;; There are no builtin `grep' in Windows, GNU's `grep' may use
  ;; the UNIX path in Windows which cannot be recognized by Emacs.
  ;; When such case occurred, we try to translate UNIX path to POSIX path.
  (defadvice compilation-find-file
      (before compilation-find-file-before disable)
    (when (string-match "^/\\([a-zA-Z]\\)/" (ad-get-arg 1))
      (ad-set-arg 1 ;; filename argument
                  (replace-match (concat (match-string 1 (ad-get-arg 1)) ":/")
                                 t t (ad-get-arg 1))))))


(defun compilation*-colorize-buffer! ()
  "Colorize *compilation* buffer."
  (when-fn% 'ansi-color-apply-on-region 'ansi-color
    (when (eq major-mode 'compilation-mode)
      (let ((inhibit-read-only t))
        (require 'ansi-color)
        (ansi-color-apply-on-region (point-min) (point-max))))))


(when-platform% 'darwin
  ;; `next-error' cannot find the source file on Darwin.
  (defun compilation*-make-change-dir (&optional buffer how)
    "Add the argument of make's -C option to `compilation-search-path'."
    (ignore* buffer how)
    (when (eq major-mode 'compilation-mode)
      (let ((d (string-match*
                ".*make.*-C[ \t]*\\([-~._/a-zA-Z0-9]+\\)\\(?:\\S*\\|.*\\)$"
                compile-command
                1)))
        (when (and d (file-exists-p d))
          (push! d compilation-search-path t))))))


(defun on-compile-init! ()
  "On \\=`compile\\=' initialization."
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
    (define-key% compilation-mode-map (kbd "g") #'recompile)
    (define-key% compilation-mode-map (kbd "q") #'quit-window))
  (setq% compilation-scroll-output t 'compile))


;; `compile' after load
(eval-after-load 'compile #'on-compile-init!)

;; `compile' global key
(define-key% (current-global-map) (kbd "C-x p c") #'compile)


(defun on-grep-init! ()
  "On \\=`grep\\=' initialization."
  ;; define `recompile' and `quit-window' key binding for `grep'
  (when-var% grep-mode-map 'grep
    (define-key% grep-mode-map (kbd "g") #'recompile)
    (define-key% grep-mode-map (kbd "q") #'quit-window)))


;;; `grep' after load
(eval-after-load 'grep #'on-grep-init!)


(defun on-make-mode-init! ()
  "On \\=`make-mode\\=' intialization."
  ;; spaced blackslash-region for makefile.
  (when-var% makefile-mode-map 'make-mode
    (when-fn% 'makefile-backslash-region 'make-mode
      (define-key% makefile-mode-map (kbd "C-c C-\\")
                   #'(lambda (from to delete-flag)
                       (interactive "r\nP")
                       (fluid-let (indent-tabs-mode nil)
                         (makefile-backslash-region from to delete-flag))))))
  (when-platform% 'darwin
    (when-fn% 'makefile-gmake-mode 'make-mode
      (when% (and (assoc** "[Mm]akefile\\'" auto-mode-alist :test #'string=)
                  (executable-find%
                   "make"
                   (lambda (make)
                     (let ((x (shell-command* make "--version")))
                       (and (zerop (car x))
                            (string-match "^GNU Make.*" (cdr x)))))))
        (when% (assoc** "[Mm]akefile\\'" auto-mode-alist :test #'string=)
          (setcdr (assoc** "[Mm]akefile\\'" auto-mode-alist :test #'string=)
                  'makefile-gmake-mode))))))

;;; `make-mode' after load
(eval-after-load 'make-mode #'on-make-mode-init!)


;; end of on-compile-autoload.el
