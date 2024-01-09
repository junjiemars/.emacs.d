;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;

;;; macro

;;; default web browser: `eww', requires Emacs-24.4+
(defmacro-if-feature% eww)
(defmacro when-feature-eww% (&rest body)
  (declare (indent 0))
  (if-feature-eww%
      `(progn% ,@body)
    `(comment ,@body)))


;;; `eglot' builtin since Emacs-29+
(defmacro-if-feature% eglot)
(defmacro when-feature-eglot% (&rest body)
  (declare (indent 0))
  (if-feature-eglot%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `project' builtin since Emacs-26+
(defmacro-if-feature% project)
(defmacro when-feature-project% (&rest body)
  (declare (indent 0))
  (if-feature-project%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `transient'
(defmacro-if-feature% transient)
(defmacro when-feature-transient% (&rest body)
  "When \\=`transient\\=', do BODY."
  (declare (indent 0))
  (if-feature-transient%
      `(progn% ,@body)
  	`(comment ,@body)))

;;; `treesit': builtin since Emacs-29+
(defmacro-if-feature% treesit)
(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (if-feature-treesit%
      `(progn% ,@body)
    `(comment ,@body)))

;; (defalias '*org-babel-schemes*
;;   (lexical-let% ((i '()))
;;     (lambda (&optional op key val)
;;       (cond ((eq :get op) (plist-get i key))
;;             ((eq :put op) (setq i (plist-put i key val)))
;;             (t i))))
;;   "The available Scheme's implementations for `ob'.")

;; end of macro

(defmacro autoload* (symbol file &optional docstring interactive type)
  "Force autoload SYMBOL, like \\=`autoload\\=' does."
  `(progn
     (fset ,symbol nil)
     (setplist ,symbol nil)
     (fset ,symbol (list 'autoload ,file ,docstring ,interactive ,type))))

;; end of macro


(defun load-autoloaded-modes! ()
  "Load autoloaded modes."
  (compile!
    (compile-unit% (emacs-home* "config/safe.el"))
    (compile-unit% (emacs-home* "config/edit.el"))
    (compile-unit% (emacs-home* "config/cc.el") t)
    (compile-unit% (emacs-home* "config/compiles.el") t)
    (compile-unit% (emacs-home* "config/direds.el") t)
    (when-platform% 'windows-nt
      (compile-unit% (emacs-home* "config/docs.el") t))
    (when-feature-eww%
      (compile-unit% (emacs-home* "config/ewws.el") t))
    (when-feature-eglot%
      (compile-unit% (emacs-home* "config/eglots.el") t))
    (compile-unit% (emacs-home* "config/eshells.el") t)
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/guds.el") t)
    (compile-unit% (emacs-home* "config/helps.el") t)
    (compile-unit% (emacs-home* "config/hippies.el") t)
    (compile-unit% (emacs-home* "config/idos.el") t)
    (compile-unit% (emacs-home* "config/lisps.el") t)
    (compile-unit% (emacs-home* "config/marks.el") t)
    (compile-unit% (emacs-home* "config/isearchs.el"))
    (compile-unit% (emacs-home* "config/mixal.el") t)
    (compile-unit% (emacs-home* "config/nets.el") t)
    (compile-unit% (emacs-home* "config/orgs.el") t)
    (compile-unit% (emacs-home* "config/pps.el") t)
    (when-feature-project%
      (compile-unit% (emacs-home* "config/projects.el") t))
    (compile-unit% (emacs-home* "config/progs.el") t)
    (compile-unit% (emacs-home* "config/pythons.el") t)
    (compile-unit% (emacs-home* "config/scratch.el"))
    (compile-unit% (emacs-home* "config/sqls.el") t)
    (compile-unit% (emacs-home* "config/tags.el"))
    (compile-unit% (emacs-home* "config/terms.el") t)
    (compile-unit% (emacs-home* "config/trans.el") t)
    (when-feature-transient%
      (compile-unit% (emacs-home* "config/transients.el") t))
    (compile-unit% (emacs-home* "config/tramps.el") t)
    (compile-unit% (emacs-home* "config/xrefs.el") t)
    (compile-unit% (emacs-home* "config/vcs.el"))))

;; end of `load-autoloaded-modes!'

(defun load-conditional-modes! ()
  "Load conditional modes."
  (compile!
    ;; on `cc'
    (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
    ;; on `compiles'
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    ;; `dict'
    (prog1
        (compile-unit% (emacs-home* "config/dict.el") t)
      (autoload 'lookup-dict (v-home%> "config/dict.el")
        "Lookup dict." t))
    ;; on `direds'
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    ;; on `docs'
    (when-platform% 'windows-nt
      (compile-unit% (emacs-home* "config/on-docview-autoload.el")))
    ;; on `eglot'
    (when-feature-eglot%
      (compile-unit% (emacs-home* "config/on-eglot-autoload.el")))
    ;; on `eshells'
    (compile-unit% (emacs-home* "config/on-eshell-autoload.el"))
    ;; on `ewws'
    (when-feature-eww%
      (compile-unit% (emacs-home* "config/on-eww-autoload.el")))
    ;; self :glyph
    (when-font%
      (compile-unit% (emacs-home* "config/glyph.el")))
    ;; `gud': `gud-cdb'
    (when-platform% 'windows-nt
      (prog1
          (compile-unit% (emacs-home* "config/gud-cdb.el") t)
        (autoload 'gud-cdb (v-home%> "config/gud-cdb.el")
          "Run cdb." t)))
    ;; `gud': `gud-lldb'
    (prog1
        (compile-unit% (emacs-home* "config/gud-lldb.el") t)
      (autoload 'gud-lldb (v-home%> "config/gud-lldb.el")
        "Run lldb." t))
    ;; on `helps'
    (compile-unit% (emacs-home* "config/on-help-autoload.el"))
    ;; on `hippies'
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    ;; on `idos'
    (compile-unit% (emacs-home* "config/on-ido-autoload.el"))
    ;; on `isearchs'
    (compile-unit% (emacs-home* "config/on-isearch-autoload.el"))
    ;; on `lisps'
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    ;; ;; `js'
    ;; (compile-unit% (emacs-home* "config/on-js-autoload.el"))
    ;; `jshell'
    (prog1
        (compile-unit% (emacs-home* "config/jshell.el") t)
      (autoload 'jshell-mode (v-home%> "config/jshell.el")
        "Toggle jshell mode." t)
      (autoload 'run-jshell (v-home%> "config/jshell.el")
        "Toggle jshell process." t))
    ;; self :key
    (when-graphic%
      (compile-unit% (emacs-home* "config/key.el")))
    ;; on `marks'
    (compile-unit% (emacs-home* "config/on-marks-autoload.el"))
    ;; `mixvm'
    (prog1
        (compile-unit% (emacs-home* "config/mixvm.el") t)
      (autoload 'mixvm (v-home%> "config/mixvm.el")
        "Run mixvm." t))
    ;; on `mixal'
    (compile-unit% (emacs-home* "config/on-mixal-autoload.el"))
    ;; `node'
    (prog1
        (compile-unit% (emacs-home* "config/node.el") t)
      (autoload 'node-mode (v-home%> "config/node.el")
        "Toggle node mode." t)
      (autoload 'run-node (v-home%> "config/node.el")
        "Toggle node process." t))
    ;; on `orgs'
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    ;; on `pps'
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    ;; on `progs'
    (compile-unit% (emacs-home* "config/on-progs-autoload.el"))
    ;; on `projects'
    (when-feature-project%
      (compile-unit% (emacs-home* "config/on-project-autoload.el")))
    ;; on `pythons'
    (compile-unit% (emacs-home* "config/on-python-autoload.el"))
    ;; `scheme': `gambit-mode'
    (prog1
        (compile-unit% (emacs-home* "config/gambit.el") t)
      (autoload 'gambit-mode (v-home%> "config/gambit.el")
        "Toggle gambit mode." t)
      (autoload* 'run-gambit (v-home%> "config/gambit.el")
                 "Toggle gambit process." t))
    ;; `scheme': `chez-mode'
    (prog1
        (compile-unit% (emacs-home* "config/chez.el") t)
      ;; (*org-babel-schemes* :put 'chez "scheme")
      (autoload 'chez-mode (v-home%> "config/chez.el")
        "Toggle chez mode." t)
      (autoload* 'run-chez (v-home%> "config/chez.el")
                 "Toggle chez process." t))
    ;; on `sqls'
    (compile-unit% (emacs-home* "config/on-sql-autoload.el"))

    ;; ;; Org `ob' for Scheme
    ;; (prog1
    ;;     (compile-unit% (emacs-home* "config/ob-schemes.el") t)
    ;;   (when (*org-babel-schemes*)
    ;;     (autoload* 'org-babel-execute:scheme*
    ;;                (v-home%> "config/ob-schemes.el")
    ;;                "Autoload `org-babel-execute:scheme*'." t)
    ;;     (fset 'org-babel-execute:scheme 'org-babel-execute:scheme*)))

    ;; `sudoku'
    (prog1
        (compile-unit% (emacs-home* "config/sudoku.el") t)
      (autoload 'sudoku (v-home%> "config/sudoku.el")
        "Play sudoku." t))
    ;; on `terms'
    (compile-unit% (emacs-home* "config/on-term-autoload.el"))
    ;; on `trans'
    (compile-unit% (emacs-home* "config/on-trans-autoload.el"))
    ;; on `transients'
    (when-feature-transient%
      (compile-unit% (emacs-home* "config/on-transient-autoload.el")))
    ;; on `treesits'
    (when-feature-treesit%
      (prog1
          (compile-unit% (emacs-home* "config/treesits.el") t)
        (autoload 'on-treesit-init! (v-home%> "config/treesits.el"))
        (compile-unit% (emacs-home* "config/on-treesit-autoload.el"))))
    ;; on `tramps'
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    ;; on `xrefs'
    (compile-unit% (emacs-home* "config/on-xref-autoload.el"))

    ) ;; end of compile!

  )
;; end of `load-conditional-modes!'


;; after-init
(defun on-autoloads! ()
  ;; preferred coding system
  (prefer-coding-system 'utf-8)
  ;; `load-path' versioned dirs
  (push! (v-home% "config/") load-path)
  (push! (v-home% "private/") load-path)
  (when-fn% 'self-socks-init! 'sockets
    (self-socks-init!))
  (when-fn% 'self-package-init! 'modules
    (self-package-init!))
  (load-autoloaded-modes!)
  (load-conditional-modes!)
  (when-fn% 'self-desktop-read! 'memo
    (condition-case err
        (self-desktop-read!)
      (error (message "self-desktop-read!: %s" err))))
  (when (*self-paths* :get :epilogue)
    (make-thread*
     (lambda ()
       (condition-case err
           (compile!
             (compile-unit* (*self-paths* :get :epilogue)))
         (error (message "self-epilogue: %s" err)))))))


;; autoload when interactive or not
(if-noninteractive%
    (on-autoloads!)
  (append! #'on-autoloads! after-init-hook))


;; end of autoloads.el
