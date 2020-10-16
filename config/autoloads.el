;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;


;; semantic, require Emacs-24.4+
(defmacro-if-feature% semantic)

;; default web browser: eww, requires Emacs-24.4+
(defmacro-if-feature% eww)


(defun set-flavor-mode! ()
  "Set flavor modes."
  (compile!
    (compile-unit% (emacs-home* "config/dicts.el"))
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/go.el") t)
    (compile-unit% (emacs-home* "config/tags.el"))
    (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    (compile-unit% (emacs-home* "config/on-edit-autoload.el"))
    (compile-unit% (emacs-home* "config/on-enc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-eshell-autoload.el"))
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    (compile-unit% (emacs-home* "config/on-indent-autoload.el"))
    (compile-unit% (emacs-home* "config/on-isearch-autoload.el"))
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-net-autoload.el"))
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-sql-autoload.el"))
    (compile-unit% (emacs-home* "config/on-term-autoload.el"))
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))

    (when-font%
      (compile-unit% (emacs-home* "config/on-glyph-autoload.el")))

    (if-feature-semantic%
        (progn
          (autoload 'set-semantic-cc-env!
            (v-home% "config/on-semantic-autoload.elc"))
          (compile-unit% (emacs-home* "config/on-semantic-autoload.el"))))

    (if-feature-eww%
        (compile-unit% (emacs-home* "config/on-eww-autoload.el")))

    (when-platform% 'windows-nt
      (when% (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c")
                 (executable-find% "mutool"))
        (compile-unit% (emacs-home* "config/on-docview-autoload.el"))))

    (when-platform% 'windows-nt
      (when% (executable-find% "cdb")
        (autoload 'cdb (v-home% "config/gud-cdb.elc")
          "Run lldb on program FILE in buffer *gud-FILE*." t)
        (compile-unit% (emacs-home* "config/gud-cdb.el") t)))

    (when% (executable-find% "lldb")
      (autoload 'lldb (v-home% "config/gud-lldb.elc")
        "Run cdb on program FILE in buffer *gud-FILE*." t)
      (compile-unit% (emacs-home* "config/gud-lldb.el") t))

    (when% (executable-find%
            "python"
            (lambda (python)
              (let ((x (shell-command* "python -c'print(1+2)'")))
                (and (zerop (car x))
                     (string= "3" (string-trim> (cdr x)))))))
      (compile-unit% (emacs-home* "config/on-python-autoload.el")))

    (when% (or (executable-find% "gsc-script")
               (executable-find% "gsc"
                                 (lambda (gsc)
                                   (let ((x (shell-command* gsc
                                              "-e \"(system-type)\"")))
                                     (zerop (car x))))))
      (compile-unit% (emacs-home* "config/gambit.el") t)
      (autoload 'gambit-mode (v-home% "config/gambit.elc")
        "Toggle Gambit's mode." t)
      (autoload 'run-gambit (v-home% "config/gambit.elc")
        "Run gambit in buffer *gambit*." t))

    (when% (executable-find% "scheme"
                             (lambda (chez)
                               (let ((x (shell-command* "echo"
                                          "'(+ 1 2 3)'|" chez "-q")))
                                 (zerop (car x)))))
      (compile-unit% (emacs-home* "config/chez.el") t)
      (autoload 'chez-mode (v-home% "config/chez.elc")
        "Toggle Chez's mode." t)
      (autoload 'run-chez (v-home% "config/chez.elc")
        "Run chez in buffer *chez*." t))

    )  ;; end of compile!

  ;; *scratch*
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (lisp-interaction-mode)))

  ;; `view-mode'
  (setq view-read-only t)

  ;; enable column number mode
  (setq% column-number-mode t 'simple)


  )

 ;; end of set-flavor-mode!


(defun set-global-key! ()
  "Set global keys."

  ;; Open file or url at point
  (when-fn% 'find-file-at-point 'ffap
    (define-key% (current-global-map) (kbd "C-c f f") #'find-file-at-point))

  ;; Shows a list of buffers
  (define-key% (current-global-map) (kbd "C-x C-b") #'ibuffer)

  ;; Interactive query replace key bindings.
  (define-key% (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key% (current-global-map) (kbd "C-M-%") #'query-replace)

  ;; Register
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key% (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)
  (define-key% (current-global-map) (kbd "C-x r v") #'view-register)

  ;; Buffer
  (define-key% (current-global-map) (kbd "C-c b r") #'revert-buffer)
  (when-fn% 'linum-mode 'linum
    (define-key% (current-global-map) (kbd "C-c b l") #'linum-mode))

  ;; Line
  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
      #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))

  ;; Sorting
  (define-key% (current-global-map) (kbd "C-c s f") #'sort-fields)
  (define-key% (current-global-map) (kbd "C-c s l") #'sort-lines)
  (define-key% (current-global-map) (kbd "C-c s r") #'reverse-region)

  ;; View
  (define-key% (current-global-map) (kbd "C-x 4 v") #'view-file-other-window)
  (define-key% (current-global-map) (kbd "C-x 5 v") #'view-file-other-frame)
  (define-key (current-global-map) (kbd "C-x C-v") #'view-file)


  ) ;; end of set-global-key!



;; after-init
(defun on-autoloads! ()
  (make-thread*
   (progn
     (compile! (compile-unit% (emacs-home* "config/sockets.el")))
     (when-package%
       (compile! (compile-unit% (emacs-home* "config/module.el"))))
     (compile! (compile-unit% (emacs-home* "config/on-module.el")))
     (package-spec-:allowed-p (apply #'compile! *autoload-compile-units*))
     (set-flavor-mode!)
     (set-global-key!)
     (compile! (compile-unit* (self-def-path-ref-> :epilogue)))
     (when-fn% 'self-desktop-read! nil (self-desktop-read!))
     (ido-mode t))
   t "on-autoloads!"))


;;; autoload when interactive or not
(if noninteractive
    (on-autoloads!)
  (add-hook 'after-init-hook #'on-autoloads! t))


;; end of autoloads.el
