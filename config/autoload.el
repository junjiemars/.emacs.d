;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoload.el
;;;;


;; set-global-key!

(defun set-global-key! ()
  
  ;; open file or url at point
  (when-fn% find-file-at-point ffap
    (define-key (current-global-map) (kbd "C-c b") #'find-file-at-point))

  
  ;; Shows a list of buffers
  (define-key (current-global-map) (kbd "C-x C-b") #'ibuffer)

  ;; regexp search and replace should be first:
  ;;
  ;; interactive search key bindings.
  ;; by default, C-s runs isearch-forward, so this swaps the bindings.
  (define-key (current-global-map) (kbd "C-s") #'isearch-forward-regexp)
  (define-key (current-global-map) (kbd "C-r") #'isearch-backward-regexp)
  (define-key (current-global-map) (kbd "C-M-s") #'isearch-forward)
  (define-key (current-global-map) (kbd "C-M-r") #'isearch-backward)

  ;; Interactive query replace key bindings.
  (define-key (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key (current-global-map) (kbd "C-M-%") #'query-replace)

  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)


  (with-eval-after-load 'grep
    (when-var% grep-mode-map grep
      ;; define `recompile' and `quit-window' key binding for `grep'
      (define-key% grep-mode-map (kbd "g") #'recompile)
      (define-key% grep-mode-map (kbd "q") #'quit-window)))


  (package-supported-p  
    (feature-allowed-p bing-dict
      ;; define `bing-dict' key binding
      (define-key (current-global-map) (kbd "C-c d") #'bing-dict-brief)))
  
  )

 ;; end of set-global-key!

;; set-flavor-mode!

(defun set-flavor-mode! ()

  (compile!
    (compile-unit% (emacs-home* "config/cc.el") t)
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/tags.el") t)
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    (compile-unit% (emacs-home* "config/on-edit-autoload.el"))
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-indent-autoload.el"))
		(compile-unit% (emacs-home* "config/on-python-autoload.el"))
    
    (platform-supported-if windows-nt
        (compile-unit% (emacs-home* "config/gud-cdb.el") t)
      
      (platform-supported-if darwin
          (compile-unit% (emacs-home* "config/gud-lldb.el") t)
        
        (when (executable-find% "lldb")
          (compile-unit% (emacs-home* "config/gud-lldb.el") t))))

    (feature-eww-supported-p
      (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
    
    (feature-linum-supported-p
      (compile-unit% (emacs-home* "config/on-linum-autoload.el")))
    
    (feature-semantic-supported-p
      (compile-unit% (emacs-home* "config/on-semantic-autoload.el"))))

  
  (autoload 'system-cc-include
    (v-home% "config/cc.elc")
    "Return a list of system include directories.")

  (platform-supported-when windows-nt
    (autoload 'make-cc-env-bat
      (v-home% "config/cc.elc")
      "Make cc-env.bat in `exec-path'."))
  
  (autoload 'set-semantic-cc-env!
    (v-home% "config/on-semantic-autoload.elc"))

  (platform-supported-when windows-nt
    ;; add .exec/ to %PATH%
    (windows-nt-env-path+ (v-home% ".exec/")))

  (platform-supported-when windows-nt
    ;; on Windows: there are no builtin zip program
    ;; so try to use minzip in Emacs dep for Windows.
    ;; zip.bat works with `dired-do-compress-to' and `org-odt-export-to-odt'.
    (eval-when-compile
      (unless (executable-find% "zip")
        ;; zip external program
        (cond ((executable-find% "7za") (make-zip-bat "7za"))
              ((executable-find% "minizip") (make-zip-bat "minizip"))))))

  )

 ;; end of set-flavor-mode!


;; set-self-epilogue!
(defun set-self-epilogue! ()
  (compile! (compile-unit* (self-def-path-ref-> :epilogue))))

 ;; end of set-self-epilogue!



;; after-init
(add-hook 'after-init-hook #'set-flavor-mode! t)
(add-hook 'after-init-hook #'set-global-key! t)
(add-hook 'after-init-hook (def-function-threading set-self-epilogue!) t)

