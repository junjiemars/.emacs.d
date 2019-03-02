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

  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)

  ;; Revert buffer
  (define-key (current-global-map) (kbd "C-c r b") #'revert-buffer)

  (package-supported-p  
    (feature-allowed-p bing-dict
      ;; define `bing-dict' key binding
      (define-key (current-global-map) (kbd "C-c d") #'bing-dict-brief)))

  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% global-map (kbd "RET") #'electric-newline-and-maybe-indent)
    (define-key% global-map (kbd "C-j") #'newline))

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
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
		(compile-unit% (emacs-home* "config/on-python-autoload.el"))
    (compile-unit% (emacs-home* "config/on-sh-autoload.el"))
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))

    (feature-eww-supported-p
      (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
    
    (feature-linum-supported-p
      (compile-unit% (emacs-home* "config/on-linum-autoload.el")))
    
    (feature-semantic-supported-p
      (compile-unit% (emacs-home* "config/on-semantic-autoload.el")))

    (platform-supported-if 'windows-nt
        (when% (executable-find% "cdb")
          (compile-unit% (emacs-home* "config/gud-cdb.el") t))
      (when% (executable-find% "lldb")
        (compile-unit% (emacs-home* "config/gud-lldb.el") t)))

    (platform-supported-when 'windows-nt
      (when% (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c")
                 (executable-find% "mutool"))
        (compile-unit% (emacs-home* "config/on-docview-autoload.el")))))


  (platform-supported-when 'windows-nt
    ;; add .exec/ to %PATH%
    (windows-nt-env-path+ (v-home% ".exec/")))

  (autoload 'set-semantic-cc-env!
    (v-home% "config/on-semantic-autoload.elc"))

  (platform-supported-if 'windows-nt
      (when% (executable-find% "cdb")
        (autoload 'cdb (v-home% "config/gud-cdb.elc")
          "Run lldb on program FILE in buffer *gud-FILE*." t))
    (when% (executable-find% "lldb")
      (autoload 'lldb (v-home% "config/gud-lldb.elc")
        "Run cdb on program FILE in buffer *gud-FILE*." t)))
  
  )

 ;; end of set-flavor-mode!


;; set-self-epilogue!
(defun set-self-epilogue! ()
  (compile! (compile-unit* (self-def-path-ref-> :epilogue))))

 ;; end of set-self-epilogue!



;; after-init
(add-hook 'after-init-hook #'set-flavor-mode! t)
(add-hook 'after-init-hook #'set-global-key! t)
(add-hook 'after-init-hook (_defun-function-threading set-self-epilogue!) t)
