;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoload.el
;;;;


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
  (define-key (current-global-map) (kbd "C-c r b") #'revert-buffer)

  (when-package%
    (if-feature-allowed% bing-dict
      ;; define `bing-dict' key binding
      (define-key (current-global-map) (kbd "C-c d") #'bing-dict-brief)))

  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
      #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))

  )

 ;; end of set-global-key!

;; set-flavor-mode!

(defun set-flavor-mode! ()

  (compile!
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/go.el") t)
    (compile-unit% (emacs-home* "config/tags.el") t)
    (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    (compile-unit% (emacs-home* "config/on-edit-autoload.el"))
    (compile-unit% (emacs-home* "config/on-eshell-autoload.el"))
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-net-autoload.el"))
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-python-autoload.el"))
    (compile-unit% (emacs-home* "config/on-sh-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))

    (if-feature-eww%
      (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
    
    (if-feature-linum%
      (compile-unit% (emacs-home* "config/on-linum-autoload.el")))
    
    (if-feature-semantic%
      (compile-unit% (emacs-home* "config/on-semantic-autoload.el")))

    (when-platform% 'windows-nt
      (when% (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c")
                 (executable-find% "mutool"))
        (compile-unit% (emacs-home* "config/on-docview-autoload.el"))))

    (when% (executable-find% "docker")
      (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))))

  ;; end of compile!

  ;; gambit
  (when% (or (executable-find% "gsc-script")
             (executable-find% "gsc"
                               (lambda (gsc)
                                 (let ((x (shell-command* gsc
                                            "-e \"(system-type)\"")))
                                   (zerop (car x))))))
    (compile!
      (compile-unit% (emacs-home* "config/gambit.el"))))

  ;; add .exec/ to %PATH%
  (when-platform% 'windows-nt
    (windows-nt-env-path+ (v-home% ".exec/")))

  ;; semantic
  (autoload 'set-semantic-cc-env!
    (v-home% "config/on-semantic-autoload.elc"))

  ;; cdb or lldb
  (if-platform% 'windows-nt
      (when% (executable-find% "cdb")
        (compile!
          (compile-unit% (emacs-home* "config/gud-cdb.el") t))
        (autoload 'cdb (v-home% "config/gud-cdb.elc")
          "Run lldb on program FILE in buffer *gud-FILE*." t))
    (when% (executable-find% "lldb")
      (compile!
        (compile-unit% (emacs-home* "config/guds.el") t)
        (compile-unit% (emacs-home* "config/gud-lldb.el") t))
      (autoload 'lldb (v-home% "config/gud-lldb.elc")
        "Run cdb on program FILE in buffer *gud-FILE*." t)))

  ;; *scratch*
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (lisp-interaction-mode)))

  )

 ;; end of set-flavor-mode!


;; set-self-epilogue!
(defun set-self-epilogue! ()
  (compile! (compile-unit* (self-def-path-ref-> :epilogue))))

 ;; end of set-self-epilogue!



;; after-init
(add-hook 'after-init-hook #'set-flavor-mode! t)
(add-hook 'after-init-hook #'set-global-key! t)
(add-hook 'after-init-hook (defun-make-thread-^fn set-self-epilogue!) t)


;; end of autoload.el
