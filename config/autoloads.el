;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;


;; default web browser: eww, requires Emacs-24.4+
(defmacro-if-feature% eww)

;; `eglot' builtin since Emacs-29+
(defmacro-if-feature% eglot)

;; `treesit': builtin since Emacs-29+
(defmacro-if-feature% treesit)


;; (defalias '*org-babel-schemes*
;;   (lexical-let% ((i '()))
;;     (lambda (&optional op key val)
;;       (cond ((eq :get op) (plist-get i key))
;;             ((eq :put op) (setq i (plist-put i key val)))
;;             (t i))))
;;   "The available Scheme's implementations for `ob'.")


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
    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/guds.el") t)
    (compile-unit% (emacs-home* "config/scratch.el"))
    (compile-unit% (emacs-home* "config/marks.el"))
    (compile-unit% (emacs-home* "config/tags.el"))
    (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    (compile-unit% (emacs-home* "config/on-help-autoload.el"))
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    (compile-unit% (emacs-home* "config/on-ido-autoload.el"))
    (compile-unit% (emacs-home* "config/on-edit-autoload.el"))
    (compile-unit% (emacs-home* "config/on-isearch-autoload.el"))
    ;; (compile-unit% (emacs-home* "config/on-js-autoload.el"))
    (compile-unit% (emacs-home* "config/on-key-autoload.el"))
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-mark-autoload.el"))
    (compile-unit% (emacs-home* "config/on-mixal-autoload.el"))
    (compile-unit% (emacs-home* "config/on-net-autoload.el"))
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-python-autoload.el"))
    (compile-unit% (emacs-home* "config/on-sql-autoload.el"))
    (compile-unit% (emacs-home* "config/on-shell-autoload.el"))
    (compile-unit% (emacs-home* "config/on-term-autoload.el"))
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-trans-autoload.el"))
    (compile-unit% (emacs-home* "config/on-transient-autoload.el"))
    (compile-unit% (emacs-home* "config/on-vc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))))
;; end of `load-autoloaded-modes!'

(defun load-conditional-modes! ()
  "Load conditional modes."
  (compile!
    ;; `dict'
    (prog1
        (compile-unit% (emacs-home* "config/dict.el") t)
      (autoload 'lookup-dict (v-home%> "config/dict.el")
        "Lookup WORD in DICT then show the result in the echo area." t))

    ;; `doc-view-mode'
    (when-platform% 'windows-nt
      (when% (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c")
                 (executable-find% "mutool"))
        (compile-unit% (emacs-home* "config/on-docview-autoload.el"))))

    ;; `glyph'
    (when-font%
      (compile-unit% (emacs-home* "config/on-glyph-autoload.el")))

    ;; `eglot'
    (if-feature-eglot%
        (compile-unit% (emacs-home* "config/on-eglot-autoload.el")))

    ;; `eww-mode'
    (if-feature-eww%
        (compile-unit% (emacs-home* "config/on-eww-autoload.el")))

    ;; Debugger `gud-cdb'
    (when-platform% 'windows-nt
      (prog1
          (compile-unit% (emacs-home* "config/gud-cdb.el") t)
        (autoload 'gud-cdb (v-home%> "config/gud-cdb.el")
          "Run lldb on program FILE in buffer *gud-FILE*." t)))

    ;; Debugger `gud-lldb'
    (prog1
        (compile-unit% (emacs-home* "config/gud-lldb.el") t)
      (autoload 'gud-lldb (v-home%> "config/gud-lldb.el")
        "Run lldb on program FILE in buffer *gud-FILE*." t))

    ;; Jshell
    (prog1
        (compile-unit% (emacs-home* "config/jshell.el") t)
      (autoload 'jshell-mode (v-home%> "config/jshell.el")
        "Toggle Jshell's mode." t)
      (autoload 'run-jshell (v-home%> "config/jshell.el")
        "Toggle jshell process in buffer \\=`*jshell*\\='." t))

    ;; `mixvm'
    (prog1
        (compile-unit% (emacs-home* "config/mixvm.el") t)
      (autoload 'mixvm (v-home%> "config/mixvm.el")
        "Run mixvm on program FILE in buffer *gud-FILE*." t))

    ;; Node
    (prog1
        (compile-unit% (emacs-home* "config/node.el") t)
      (autoload 'node-mode (v-home%> "config/node.el")
        "Toggle Node's mode." t)
      (autoload 'run-node (v-home%> "config/node.el")
        "Toggle node process in buffer \\=`*node*\\='." t))

    ;; Scheme `gambit-mode'
    (prog1
        (compile-unit% (emacs-home* "config/gambit.el") t)
      (autoload 'gambit-mode (v-home%> "config/gambit.el")
        "Toggle Gambit's mode." t)
      (autoload* 'run-gambit (v-home%> "config/gambit.el")
                 "Toggle gambit process in buffer \\=`*gambit*\\='." t))

    ;; Scheme `chez-mode'
    (prog1
        (compile-unit% (emacs-home* "config/chez.el") t)
      ;; (*org-babel-schemes* :put 'chez "scheme")
      (autoload 'chez-mode (v-home%> "config/chez.el")
        "Toggle Chez's mode." t)
      (autoload* 'run-chez (v-home%> "config/chez.el")
                 "Toggle chez process in buffer \\=`*chez*\\='." t))

    ;; ;; Org `ob' for Scheme
    ;; (prog1
    ;;     (compile-unit% (emacs-home* "config/ob-schemes.el") t)
    ;;   (when (*org-babel-schemes*)
    ;;     (autoload* 'org-babel-execute:scheme*
    ;;                (v-home%> "config/ob-schemes.el")
    ;;                "Autoload `org-babel-execute:scheme*'." t)
    ;;     (fset 'org-babel-execute:scheme 'org-babel-execute:scheme*)))

    ;; Sudoku
    (prog1
        (compile-unit% (emacs-home* "config/sudoku.el") t)
      (autoload 'sudoku (v-home%> "config/sudoku.el")
        "Play sudoku." t))

    ;; `treesit'
    (if-feature-treesit%
        (compile-unit% (emacs-home* "config/on-treesit-autoload.el")))

    ) ;; end of compile!

  )
;; end of `load-conditional-modes!'


;; after-init
(defun on-autoloads! ()
  ;; preferred coding system
  (prefer-coding-system 'utf-8)
  (compile!
    (when (*self-env-spec* :get :socks :allowed)
      (compile-unit% (emacs-home* "config/sockets.el")))
    (when-package%
      (when (*self-env-spec* :get :package :allowed)
        (compile-unit% (emacs-home* "config/packages.el")))))
  ;; `load-path' versioned dirs
  (push! (v-home% "config/") load-path)
  (push! (v-home% "private/") load-path)
  (load-autoloaded-modes!)
  (load-conditional-modes!)
  (when-fn% 'toggle-frame-initialized 'graphic
    (toggle-frame-initialized))
  (when-fn% 'self-desktop-read! 'memo
    (condition-case err
        (self-desktop-read!)
      (error (message "self-desktop-read!: %s" err))))
  (compile!
    (compile-unit% (emacs-home* "config/on-inter-autoload.el")))
  (when (*self-paths* :get :epilogue)
    (make-thread*
     (lambda ()
       (condition-case err
           (compile!
             (compile-unit* (*self-paths* :get :epilogue)))
         (error (message ":epilogue: %s" err)))))))


;; autoload when interactive or not
(if-noninteractive%
    (on-autoloads!)
  (append! #'on-autoloads! after-init-hook))


;; end of autoloads.el
