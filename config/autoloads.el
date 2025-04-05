;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;

;;; require

;; (defalias '*org-babel-schemes*
;;   (let ((i '()))
;;     (lambda (&optional op key val)
;;       (cond ((eq :get op) (plist-get i key))
;;             ((eq :put op) (setq i (plist-put i key val)))
;;             (t i))))
;;   "The available Scheme's implementations for `ob'.")

;; end of require

(defun load-prologue-modes! ()
  "Load prologue modes."
  (compile!
   (compile-unit% (emacs-home* "config/cls.el") t)
   (compile-unit% (emacs-home* "config/ed.el") t)
   (compile-unit% (emacs-home* "config/marks.el") t)
   (compile-unit% (emacs-home* "config/ssh.el") t)
   (compile-unit% (emacs-home* "config/tags.el") t)
   (compile-unit% (emacs-home* "config/thingatpts.el") t)
   (compile-unit% (emacs-home* "config/on-firstload.el"))))

(defun load-compiling-modes! ()
  "Load compiling modes."
  (compile!
   (compile-unit% (emacs-home* "config/cc.el") t)
   (unless-graphic%
     (compile-unit% (emacs-home* "config/clipboard.el") t))
   (compile-unit% (emacs-home* "config/chez.el") t)
   (compile-unit% (emacs-home* "config/compiles.el") t)
   (compile-unit% (emacs-home* "config/cscope.el") t)
   (compile-unit% (emacs-home* "config/dict.el") t)
   (compile-unit% (emacs-home* "config/direds.el") t)
   (when-platform% windows-nt
     (compile-unit% (emacs-home* "config/docs.el") t))
   (when-feature% eglot
     (compile-unit% (emacs-home* "config/eglots.el") t))
   (compile-unit% (emacs-home* "config/elisps.el") t)
   (compile-unit% (emacs-home* "config/eshells.el") t)
   (when-feature% eww
     (compile-unit% (emacs-home* "config/ewws.el") t))
   (compile-unit% (emacs-home* "config/financial.el") t)
   (compile-unit% (emacs-home* "config/helps.el") t)
   (compile-unit% (emacs-home* "config/hippies.el") t)
   (compile-unit% (emacs-home* "config/isearchs.el") t)
   (compile-unit% (emacs-home* "config/jshell.el") t)
   ;; `scheme': `gambit-mode' abandoned
   ;; (compile-unit% (emacs-home* "config/gambit.el") t)
   (when-platform% windows-nt
     (compile-unit% (emacs-home* "config/gud-cdb.el") t))
   (compile-unit% (emacs-home* "config/gud-lldb.el") t)
   (compile-unit% (emacs-home* "config/mixal.el") t)
   (compile-unit% (emacs-home* "config/node.el") t)
   (compile-unit% (emacs-home* "config/orgs.el") t)
   (when-feature% project
     (compile-unit% (emacs-home* "config/projects.el") t))
   (compile-unit% (emacs-home* "config/progs.el") t)
   (compile-unit% (emacs-home* "config/pythons.el") t)
   (compile-unit% (emacs-home* "config/scratch.el") t)
   (compile-unit% (emacs-home* "config/sqls.el") t)
   (compile-unit% (emacs-home* "config/sudoku.el") t)
   (compile-unit% (emacs-home* "config/terms.el") t)
   (compile-unit% (emacs-home* "config/trans.el") t)
   (when-feature% transient
     (compile-unit% (emacs-home* "config/transients.el") t))
   (when-feature-treesit%
    (compile-unit% (emacs-home* "config/treesits.el") t))
   (compile-unit% (emacs-home* "config/tramps.el") t)
   (compile-unit% (emacs-home* "config/xrefs.el") t)
   (when-feature-vc%
     (compile-unit% (emacs-home* "config/vcs.el") t))))

;; end of `load-compiling-modes!'

(defun load-conditional-modes! ()
  "Load conditional modes."
  (compile!
   ;; on `cc'
   (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
   ;; ;; on `clipboard'
   ;; (unless-graphic%
   ;;   (compile-unit% (emacs-home* "config/on-clipboard-autoload.el")))
   ;; on `compiles'
   (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
   ;; on `direds'
   (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
   ;; on `docs'
   (when-platform% windows-nt
     (compile-unit% (emacs-home* "config/on-docview-autoload.el")))
   ;; on `elisp'
   (compile-unit% (emacs-home* "config/on-elisp-autoload.el"))
   ;; on `eglot'
   (when-feature% eglot
     (compile-unit% (emacs-home* "config/on-eglot-autoload.el")))
   ;; on `ewws'
   (when-feature% eww
     (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
   ;; self : `glyph'
   (when-graphic%
     (compile-unit% (emacs-home* "config/glyph.el")))
   ;; on `helps'
   (compile-unit% (emacs-home* "config/on-help-autoload.el"))
   ;; on `hippies'
   (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
   ;; `js'
   ;; (compile-unit% (emacs-home* "config/on-js-autoload.el"))
   ;; on `mixal'
   (compile-unit% (emacs-home* "config/on-mixal-autoload.el"))
   ;; on `orgs'
   (compile-unit% (emacs-home* "config/on-org-autoload.el"))
   ;; on `projects'
   (when-feature% project
     (compile-unit% (emacs-home* "config/on-project-autoload.el")))
   ;; on `pythons'
   (compile-unit% (emacs-home* "config/on-python-autoload.el"))
   ;; on `sqls'
   (compile-unit% (emacs-home* "config/on-sql-autoload.el"))
   ;; ;; Org `ob' for Scheme
   ;; (prog1
   ;;     (compile-unit% (emacs-home* "config/ob-schemes.el") t)
   ;;   (when (*org-babel-schemes*)
   ;;     (autoload! 'org-babel-execute:scheme*
   ;;                (v-home%> "config/ob-schemes")
   ;;                "Autoload `org-babel-execute:scheme*'." t)
   ;;     (fset 'org-babel-execute:scheme 'org-babel-execute:scheme*)))
   ;; on `terms'
   (compile-unit% (emacs-home* "config/on-term-autoload.el"))
   ;; on `transients'
   (when-feature% transient
     (compile-unit% (emacs-home* "config/on-transient-autoload.el")))
   ;; on `treesits'
   (when-feature-treesit%
    (compile-unit% (emacs-home* "config/on-treesit-autoload.el")))
   ;; on `tramps'
   (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
   ;; on `xrefs'
   (compile-unit% (emacs-home* "config/on-xref-autoload.el"))
   ;; on `vcs'
   (when-feature-vc%
     (compile-unit% (emacs-home* "config/on-vcs-autoload.el")))
   ;; `windows'
   (compile-unit% (emacs-home* "config/windows.el"))
   ;; on last:
   (compile-unit% (emacs-home* "config/on-lastload.el"))
   ) ;; end of compile!
  )
;; end of `load-conditional-modes!'

(defun self-epilogue-init! ()
  (condition-case err
      (compile! (compile-unit* (*self-paths* :get :epilogue)))
    (error "self-epilogue: %s" err)))

;; end of `self-epilogue-init!'

;; after-init
(defun on-autoloads! ()
  "Autoload after init."
  (self-graphic-init!)
  (when-fn% self-shell-read! nil (self-shell-read!))
  (when-fn% self-socks-init! nil (self-socks-init!))
  (setq% history-length (emacs-arch%))
  (setq% message-log-max 512)
  ;; preferred coding system
  (prefer-coding-system 'utf-8)
  (load-prologue-modes!)
  (load-compiling-modes!)
  (load-conditional-modes!)
  (when-fn% self-edit-init! nil (self-edit-init!))
  (when-fn% self-key-init! nil (self-key-init!))
  (when-fn% self-glyph-init! nil (make-thread* #'self-glyph-init!))
  (when-fn% self-module-init! nil
    (condition-case err
        (self-module-init!)
      (error "self-module-init!: %s" err)))
  (when-fn% self-desktop-read! nil
    (condition-case err
        (self-desktop-read!)
      (error "self-desktop-read!: %s" err)))
  ;; `load-path' versioned dirs
  (push! (v-home% "config/") load-path)
  (push! (v-home% "private/") load-path)
  (when (*self-paths* :get :epilogue)
    (make-thread* #'self-epilogue-init! (unless-interactive% t))))

;;; autoload when interactive or not
(if-interactive%
    (append! #'on-autoloads! after-init-hook)
  (on-autoloads!))

;; end of autoloads.el
