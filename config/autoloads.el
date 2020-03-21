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


;; set-global-key!

(defun set-global-key! ()

  ;; Open file or url at point
  (when-fn% 'find-file-at-point 'ffap
    (define-key (current-global-map) (kbd "C-c f f") #'find-file-at-point))

  ;; Shows a list of buffers
  (define-key (current-global-map) (kbd "C-x C-b") #'ibuffer)

  ;; Interactive query replace key bindings.
  (define-key (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key (current-global-map) (kbd "C-M-%") #'query-replace)

  ;; Register
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)
  (define-key% (current-global-map) (kbd "C-x r v") #'view-register)

  ;; Revert buffer
  (define-key (current-global-map) (kbd "C-c b r") #'revert-buffer)
  ;; Toggle linum mode
  (when-fn% 'linum-mode 'linum
    (define-key% (current-global-map) (kbd "C-c b l") #'linum-mode))

  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
      #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))

  ) ;; end of set-global-key!



;; set-flavor-mode!

(defun set-flavor-mode! ()

  (compile!
    (compile-unit% (emacs-home* "config/dicts.el"))
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/fns.el"))
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
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-net-autoload.el"))
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))

    (when-font%
      (compile-unit% (emacs-home* "config/on-font-autoload.el")))

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
      (autoload 'gambit-mode (v-home% "config/gambit.elc")
        "Toggle Gambit's mode." t)
      (autoload 'run-gambit (v-home% "config/gambit.elc")
        "Run gambit in buffer *gambit*." t)
      (compile-unit% (emacs-home* "config/gambit.el") t))

    )  ;; end of compile!

  ;; add .exec/ to %PATH%
  (when-platform% 'windows-nt
    (windows-nt-env-path+ (v-home% ".exec/")))

  ;; *scratch*
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (lisp-interaction-mode)))

  )

 ;; end of set-flavor-mode!


;; after-init
(defun on-autoloads! ()
  (make-thread* (progn%
                 (set-flavor-mode!)
                 (set-global-key!)
                 (package-spec-:allowed-p
                   (apply #'compile! *autoload-compile-units*))
                 (compile! (compile-unit*
                            (self-def-path-ref-> :epilogue)))
                 (self-desktop-read!))
                t "on-autoloads!"))

(add-hook 'after-init-hook #'on-autoloads! t)


;; end of autoloads.el
