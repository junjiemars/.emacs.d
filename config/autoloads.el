;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;

;;; macro

;; (defalias '*org-babel-schemes*
;;   (lexical-let% ((i '()))
;;     (lambda (&optional op key val)
;;       (cond ((eq :get op) (plist-get i key))
;;             ((eq :put op) (setq i (plist-put i key val)))
;;             (t i))))
;;   "The available Scheme's implementations for `ob'.")


(defmacro autoload! (symbol file &optional docstring interactive type)
  "Force autoload SYMBOL."
  (declare (indent 0))
  `(progn
     (fset ,symbol nil)
     (setplist ,symbol nil)
     (fset ,symbol (list 'autoload ,file ,docstring ,interactive ,type))))

;; end of macro

(defun load-prologue-modes! ()
  "Load prologue modes."
  (compile!
    (compile-unit% (emacs-home* "config/cls.el") t)
    (compile-unit% (emacs-home* "config/ed.el") t)
    (compile-unit% (emacs-home* "config/marks.el") t)
    (compile-unit% (emacs-home* "config/tags.el") t)
    (compile-unit% (emacs-home* "config/on-firstload.el"))))

(defun load-compiling-modes! ()
  "Load compiling modes."
  (compile!
    (compile-unit% (emacs-home* "config/cc.el") t)
    (unless-graphic%
      (compile-unit% (emacs-home* "config/clipboard.el") t))
    (compile-unit% (emacs-home* "config/compiles.el") t)
    (compile-unit% (emacs-home* "config/direds.el") t)
    (when-platform% 'windows-nt
      (compile-unit% (emacs-home* "config/docs.el") t))
    (when-feature-eglot%
      (compile-unit% (emacs-home* "config/eglots.el") t))
    (compile-unit% (emacs-home* "config/elisps.el") t)
    (compile-unit% (emacs-home* "config/eshells.el") t)
    (when-feature-eww%
      (compile-unit% (emacs-home* "config/ewws.el") t))
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/guds.el") t)
    (compile-unit% (emacs-home* "config/helps.el") t)
    (compile-unit% (emacs-home* "config/hippies.el") t)
    (compile-unit% (emacs-home* "config/idos.el") t)
    (compile-unit% (emacs-home* "config/isearchs.el") t)
    (compile-unit% (emacs-home* "config/mill.el") t)
    (compile-unit% (emacs-home* "config/mixal.el") t)
    (compile-unit% (emacs-home* "config/nets.el") t)
    (compile-unit% (emacs-home* "config/orgs.el") t)
    (when-feature-project%
      (compile-unit% (emacs-home* "config/projects.el") t))
    (compile-unit% (emacs-home* "config/progs.el") t)
    (compile-unit% (emacs-home* "config/pythons.el") t)
    (compile-unit% (emacs-home* "config/sqls.el") t)
    (compile-unit% (emacs-home* "config/terms.el") t)
    (compile-unit% (emacs-home* "config/trans.el") t)
    (when-feature-transient%
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
    ;; `dict'
    (prog1
        (compile-unit% (emacs-home* "config/dict.el") t)
      (autoload 'lookup-dict (v-home%> "config/dict")
        "Lookup dict." t))
    ;; on `direds'
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    ;; on `docs'
    (when-platform% 'windows-nt
      (compile-unit% (emacs-home* "config/on-docview-autoload.el")))
    ;; on `elisp'
    (compile-unit% (emacs-home* "config/on-elisp-autoload.el"))
    ;; on `eglot'
    (when-feature-eglot%
      (compile-unit% (emacs-home* "config/on-eglot-autoload.el")))
    ;; on `ewws'
    (when-feature-eww%
      (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
    ;; self :glyph
    (when-font%
      (prog1
          (compile-unit% (emacs-home* "config/glyph.el") t)
        (autoload 'self-glyph-init! (v-home%> "config/glyph"))
        (declare-function self-glyph-init! (v-home%> "config/glyph"))))
    ;; `gud': `gud-cdb'
    (when-platform% 'windows-nt
      (prog1
          (compile-unit% (emacs-home* "config/gud-cdb.el") t)
        (autoload 'gud-cdb (v-home%> "config/gud-cdb")
          "Run cdb." t)))
    ;; `gud': `gud-lldb'
    (prog1
        (compile-unit% (emacs-home* "config/gud-lldb.el") t)
      (autoload 'gud-lldb (v-home%> "config/gud-lldb")
        "Run lldb." t))
    ;; on `helps'
    (compile-unit% (emacs-home* "config/on-help-autoload.el"))
    ;; on `hippies'
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    ;; on `idos'
    (compile-unit% (emacs-home* "config/on-ido-autoload.el"))
    ;; on `isearchs'
    (compile-unit% (emacs-home* "config/on-isearch-autoload.el"))
    ;; ;; `js'
    ;; (compile-unit% (emacs-home* "config/on-js-autoload.el"))
    ;; `jshell'
    (prog1
        (compile-unit% (emacs-home* "config/jshell.el") t)
      (autoload 'jshell-mode (v-home%> "config/jshell")
        "Toggle jshell mode." t)
      (autoload 'run-jshell (v-home%> "config/jshell")
        "Toggle jshell process." t))
    ;; `mixvm'
    (prog1
        (compile-unit% (emacs-home* "config/mixvm.el") t)
      (autoload 'mixvm (v-home%> "config/mixvm")
        "Run mixvm." t))
    ;; on `mixal'
    (compile-unit% (emacs-home* "config/on-mixal-autoload.el"))
    ;; `node'
    (prog1
        (compile-unit% (emacs-home* "config/node.el") t)
      (autoload 'node-mode (v-home%> "config/node")
        "Toggle node mode." t)
      (autoload 'run-node (v-home%> "config/node")
        "Toggle node process." t))
    ;; on `orgs'
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    ;; on `projects'
    (when-feature-project%
      (compile-unit% (emacs-home* "config/on-project-autoload.el")))
    ;; on `pythons'
    (compile-unit% (emacs-home* "config/on-python-autoload.el"))
    ;; `scheme': `gambit-mode'
    (prog1
        (compile-unit% (emacs-home* "config/gambit.el") t)
      (autoload 'gambit-mode (v-home%> "config/gambit")
        "Toggle gambit mode." t)
      (autoload! 'run-gambit (v-home%> "config/gambit")
                 "Toggle gambit process." t))
    ;; `scheme': `chez-mode'
    (prog1
        (compile-unit% (emacs-home* "config/chez.el") t)
      ;; (*org-babel-schemes* :put 'chez "scheme")
      (autoload 'chez-mode (v-home%> "config/chez")
        "Toggle chez mode." t)
      (autoload! 'run-chez (v-home%> "config/chez")
                 "Toggle chez process." t))
    ;; `scratch'
    (prog1
        (compile-unit% (emacs-home* "config/scratch.el") t)
      (autoload 'scratch (v-home%> "config/scratch")
        "Scratch" t))
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

    ;; `sudoku'
    (prog1
        (compile-unit% (emacs-home* "config/sudoku.el") t)
      (autoload 'sudoku (v-home%> "config/sudoku")
        "Play sudoku." t))
    ;; on `terms'
    (compile-unit% (emacs-home* "config/on-term-autoload.el"))
    ;; on `transients'
    (when-feature-transient%
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
    ;; on last:
    (compile-unit% (emacs-home* "config/on-lastload.el"))
    ) ;; end of compile!
  )
;; end of `load-conditional-modes!'


;; after-init
(defun on-autoloads! ()
  "Autoload after init."
  ;; preferred coding system
  (prefer-coding-system 'utf-8)
  (self-graphic-init!)
  (when-fn% 'self-shell-read! nil (self-shell-read!))
  (when-fn% 'self-socks-init! nil (self-socks-init!))
  (setq% history-length (emacs-arch))
  (setq% message-log-max 512)
  (load-prologue-modes!)
  (load-compiling-modes!)
  (load-conditional-modes!)
  (when-fn% 'self-edit-init! nil (self-edit-init!))
  (when-fn% 'self-key-init! nil (self-key-init!))
  (when-font% (make-thread* #'self-glyph-init!))
  (when-fn% 'self-module-init! nil
    (condition-case err
        (self-module-init!)
      (error "self-module-init!: %s" err)))
  (when-fn% 'self-desktop-read! nil
    (condition-case err
        (self-desktop-read!)
      (error "self-desktop-read!: %s" err)))
  ;; `load-path' versioned dirs
  (push! (v-home% "config/") load-path)
  (push! (v-home% "private/") load-path)
  (when (*self-paths* :get :epilogue)
    (make-thread*
     (lambda () (thread-yield*)
       (condition-case err
           (compile! (compile-unit* (*self-paths* :get :epilogue)))
         (error "self-epilogue: %s" err)))
     (if-noninteractive% t))))

;;; autoload when interactive or not
(if-noninteractive%
    (on-autoloads!)
  (append! #'on-autoloads! after-init-hook))

;; end of autoloads.el
