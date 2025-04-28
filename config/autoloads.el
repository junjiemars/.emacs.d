;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;

;;; recast
;; 1. config/ob-schemes.el: `ob-schemes', Org `ob' for Scheme.
;; 2. config/mixal.el
;; 3. config/mixvm.el
;;
;; end of recast

;;; abandoned
;; 1. config/gambit.el: `gambit-mode'.
;; 2. config/on-js-autoload.el.
;;
 ;; end of abandoned

(defun define-prologue-mode ()
  "Define prologue modes."
  (compile!
   (compile-unit% (emacs-home* "config/cls.el") t)
   (unless-graphic%
     (compile-unit% (emacs-home* "config/clipboard.el") t))
   (compile-unit% (emacs-home* "config/ed.el") t)
   (compile-unit% (emacs-home* "config/marks.el") t)
   (compile-unit% (emacs-home* "config/ssh.el") t)
   (compile-unit% (emacs-home* "config/tags.el") t)
   (compile-unit% (emacs-home* "config/thingatpts.el") t)))

;; end of `define-prologue-mode'

(defun define-compile-only-mode ()
  "Define compile-only modes."
  (compile!
   (compile-unit% (emacs-home* "config/chez.el") t)
   (compile-unit% (emacs-home* "config/cscope.el") t)
   (compile-unit% (emacs-home* "config/dict.el") t)
   (compile-unit% (emacs-home* "config/financial.el") t)
   (compile-unit% (emacs-home* "config/isearchs.el") t)
   (compile-unit% (emacs-home* "config/jshell.el") t)
   (when-platform% windows-nt
     (compile-unit% (emacs-home* "config/gud-cdb.el") t))
   (compile-unit% (emacs-home* "config/gud-lldb.el") t)
   (compile-unit% (emacs-home* "config/direds-ls.el") t)
   (compile-unit% (emacs-home* "config/node.el") t)
   (compile-unit% (emacs-home* "config/scratch.el") t)
   (compile-unit% (emacs-home* "config/sudoku.el") t)
   (compile-unit% (emacs-home* "config/trans.el") t)
   (when-feature-treesit%
     (compile-unit% (emacs-home* "config/treesits.el") t))
   (when-feature-vc%
     (compile-unit% (emacs-home* "config/vcs.el") t))
   (unless-fn% recenter-top-bottom nil
     (compile-unit% (emacs-home* "config/winmoves.el") t))))

;; end of `define-compile-only-mode'

(defun define-after-load-mode ()
  "Load autoload modes."
  (compile!
   (compile-unit% (emacs-home* "config/cc.el") t)
   (compile-unit% (emacs-home* "config/compiles.el") t)
   (when-platform% windows-nt
     (compile-unit% (emacs-home* "config/docs.el") t))
   (when-feature% eglot
     (compile-unit% (emacs-home* "config/eglots.el") t))
   (compile-unit% (emacs-home* "config/elisps.el") t)
   (compile-unit% (emacs-home* "config/eshells.el") t)
   (when-feature% eww
     (compile-unit% (emacs-home* "config/ewws.el") t))
   (compile-unit% (emacs-home* "config/helps.el") t)
   (compile-unit% (emacs-home* "config/hippies.el") t)
   (compile-unit% (emacs-home* "config/direds.el") t)
   (compile-unit% (emacs-home* "config/orgs.el") t)
   (when-feature% project
     (compile-unit% (emacs-home* "config/projects.el") t))
   (compile-unit% (emacs-home* "config/progs.el") t)
   (compile-unit% (emacs-home* "config/pythons.el") t)
   (compile-unit% (emacs-home* "config/sqls.el") t)
   (compile-unit% (emacs-home* "config/terms.el") t)
   (when-feature% transient
     (compile-unit% (emacs-home* "config/transients.el") t))
   (compile-unit% (emacs-home* "config/tramps.el") t)
   (compile-unit% (emacs-home* "config/xrefs.el") t)))

;; end of `define-after-load-mode'

(defun declare-prologue-mode! ()
  ;; `cls'
  (autoload 'assoc** (v-home%> "config/cls"))
  (autoload 'drop (v-home%> "config/cls"))
  (autoload 'every* (v-home%> "config/cls"))
  (autoload 'insert-at (v-home%> "config/cls"))
  ;; (autoload 'insert! (v-home%> "config/cls") nil nil 'macro)
  (autoload 'loop* (v-home%> "config/cls"))
  (autoload 'mapcar** (v-home%> "config/cls"))
  (autoload 'member-if* (v-home%> "config/cls"))
  (autoload 'remove-if* (v-home%> "config/cls"))
  (autoload 'range (v-home%> "config/cls"))
  (autoload 'some* (v-home%> "config/cls"))
  (unless-fn% take nil (autoload 'take (v-home%> "config/cls")))
  (autoload 'take-while (v-home%> "config/cls"))
  ;; `clipboard'
  (unless-graphic%
    (declare-function on-clipboard-init! (v-home%> "config/clipboard"))
    (autoload 'on-clipboard-init! (v-home%> "config/clipboard"))
    (make-thread* #'on-clipboard-init!))
  ;; `ed'
  (autoload 'delete-line* (v-home%> "config/ed"))
  (autoload 'file-in-dirs-p (v-home%> "config/ed"))
  (autoload 'newline* (v-home%> "config/ed") nil t)
  (autoload 'open-next-line (v-home%> "config/ed") nil t)
  (autoload 'open-previous-line (v-home%> "config/ed") nil t)
  (autoload 'read-string-prompt (v-home%> "config/ed"))
  (autoload 'select-region-prompt (v-home%> "config/ed"))
  (autoload 'vstrncmp (v-home%> "config/ed"))
  ;; `marks'
  (declare-function symbol@* (v-home%> "config/marks"))
  (declare-function kill-sexp@ (v-home%> "config/marks") nil t)
  (declare-function kill-string@ (v-home%> "config/marks") nil t)
  (declare-function kill-word@ (v-home%> "config/marks") nil t)
  (declare-function mark-defun@ (v-home%> "config/marks") nil t)
  (declare-function mark-filename@ (v-home%> "config/marks") nil t)
  (declare-function mark-line@ (v-home%> "config/marks") nil t)
  (declare-function mark-sexp@ (v-home%> "config/marks") nil t)
  (declare-function mark-string@ (v-home%> "config/marks") nil t)
  (declare-function mark-symbol@ (v-home%> "config/marks") nil t)
  (declare-function mark-word@ (v-home%> "config/marks") nil t)
  (autoload 'symbol@* (v-home%> "config/marks"))
  (autoload 'kill-sexp@ (v-home%> "config/marks") nil t)
  (autoload 'kill-string@ (v-home%> "config/marks") nil t)
  (autoload 'kill-word@ (v-home%> "config/marks") nil t)
  (autoload 'mark-defun@ (v-home%> "config/marks") nil t)
  (autoload 'mark-filename@ (v-home%> "config/marks") nil t)
  (autoload 'mark-line@ (v-home%> "config/marks") nil t)
  (autoload 'mark-sexp@ (v-home%> "config/marks") nil t)
  (autoload 'mark-string@ (v-home%> "config/marks") nil t)
  (autoload 'mark-symbol@ (v-home%> "config/marks") nil t)
  (autoload 'mark-word@ (v-home%> "config/marks") nil t)
  ;; `ssh'
  (autoload 'ssh-remote-p (v-home%> "config/ssh"))
  (autoload 'ssh-remote->ids (v-home%> "config/ssh"))
  (autoload 'ssh-remote->user@host (v-home%> "config/ssh"))
  (autoload 'ssh-remote-command (v-home%> "config/ssh"))
  ;; `tags'
  (autoload 'tags-spec->* (v-home%> "config/tags"))
  (autoload 'make-c-tags (v-home%> "config/tags"))
  (autoload 'make-dir-ctags (v-home%> "config/tags"))
  (autoload 'make-dir-tags (v-home%> "config/tags") nil t)
  (autoload 'mount-tags (v-home%> "config/tags") nil t)
  (autoload 'unmount-tags (v-home%> "config/tags") nil t)
  ;; `thingsatpts'
  (declare-function on-thingatpt-init! (v-home%> "config/thingatpts"))
  (autoload 'on-thingatpt-init! (v-home%> "config/thingatpts"))
  (with-eval-after-load 'thingatpt
    (make-thread* #'on-thingatpt-init!)))

(defun declare-compile-only-mode! ()
  ;; `chez'
  (autoload 'chez-mode (v-home%> "config/chez") "Toggle chez mode." t)
  (autoload 'run-chez (v-home%> "config/chez") "Run chez REPL." t)
  ;; `cscope'
  (autoload 'cscope (v-home%> "config/cscope") "Run cscope." t)
  (autoload 'run-cscope (v-home%> "config/cscope") "Run cscope REPL." t)
  ;; `dict'
  (autoload 'lookup-dict (v-home%> "config/dict") "Lookup dict." t)
  (define-key (current-global-map) (kbd% "M-s d") 'lookup-dict)
  ;; `isearchs'
  (declare-function isearch*-forward (v-home%> "config/isearchs"))
  (declare-function isearch*-backward (v-home%> "config/isearchs"))
  (declare-function isearch*-forward-symbol (v-home%> "config/isearchs"))
  (autoload 'isearch*-forward (v-home%> "config/isearchs"))
  (autoload 'isearch*-backward (v-home%> "config/isearchs"))
  (autoload 'isearch*-forward-symbol (v-home%> "config/isearchs"))
  ;; `jshell'
  (autoload 'jshell-mode (v-home%> "config/jshell") "Toggle jshell mode." t)
  (autoload 'run-jshell (v-home%> "config/jshell") "Run jshell REPL" t)
  (push! `("\\.jsh\\'" . java-mode) auto-mode-alist)
  ;; `gud-cdb'
  (when-platform% windows-nt
    (autoload 'gud-cdb (v-home%> "config/gud-cdb") "Run cdb." t))
  ;; `gud-lldb'
  (autoload 'gud-lldb (v-home%> "config/gud-lldb") "Run lldb." t)
  ;; `direds-ls'
  (declare-function on-direds-ls-init! (v-home%> "config/direds-ls"))
  (autoload 'on-direds-ls-init! (v-home%> "config/direds-ls"))
  (make-thread* #'on-direds-ls-init!)
  ;; `node'
  (autoload 'node-mode (v-home%> "config/node") "Toggle node mode." t)
  (autoload 'run-node (v-home%> "config/node") "Run node REPL." t)
  ;; `scratch'
  (autoload 'scratch (v-home%> "config/scratch") "Scratch" t)
  ;; `sudoku'
  (autoload 'sudoku (v-home%> "config/sudoku") "Play sudoku." t)
  ;; `trans'
  (autoload 'ascii-table (v-home%> "config/trans") nil t)
  (autoload 'chinese->arabic (v-home%> "config/trans") nil t)
  (autoload 'decode-base64 (v-home%> "config/trans") nil t)
  (autoload 'decode-chinese-number (v-home%> "config/trans") nil t)
  (autoload 'decode-ipv4 (v-home%> "config/trans") nil t)
  (autoload 'decode-roman-number (v-home%> "config/trans") nil t)
  (autoload 'decode-url (v-home%> "config/trans") nil t)
  (autoload 'encode-base64 (v-home%> "config/trans") nil t)
  (autoload 'encode-ipv4 (v-home%> "config/trans") nil t)
  (autoload 'encode-url (v-home%> "config/trans") nil t)
  (autoload 'greek-alphabet (v-home%> "config/trans") nil t)
  (autoload 'int->ipv4 (v-home%> "config/trans") nil t)
  (autoload 'ipv4->int (v-home%> "config/trans") nil t)
  (autoload 'roman->arabic (v-home%> "config/trans") nil t)
  ;; `treesits'
  (when-feature-treesit%
    (declare-function toggle-treesit! (v-home%> "config/treesits"))
    (autoload 'toggle-treesit! (v-home%> "config/treesits") nil t
      "Toggle treesit."))
  ;; `vcs'
  (when-feature-vc%
    (declare-function vc*-dir (v-home%> "config/vcs"))
    (autoload 'vc*-dir (v-home%> "config/vcs") nil t))
  (unless-fn% recenter-top-bottom nil
    (declare-function recenter-top-bottom (v-home%> "config/winmoves"))
    (autoload 'recenter-top-bottom (v-home%> "config/winmoves"))))

(defun declare-after-load-mode! ()
  ;; `cc'
  (declare-function on-cc-mode-init! (v-home%> "config/cc"))
  (declare-function on-man-init! (v-home%> "config/cc"))
  (autoload 'on-cc-mode-init! (v-home%> "config/cc"))
  (autoload 'on-man-init! (v-home%> "config/cc"))
  (autoload 'cc*-cc (v-home%> "config/cc"))
  (autoload 'cc*-system-include (v-home%> "config/cc"))
  (autoload 'cc*-make-tags (v-home%> "config/cc"))
  (with-eval-after-load 'cc-mode
    (make-thread* #'on-cc-mode-init!))
  (with-eval-after-load 'man
    (make-thread* #'on-man-init!))
  ;; `compiles'
  (declare-function on-compile-init! (v-home%> "config/compiles"))
  (declare-function on-grep-init! (v-home%> "config/compiles"))
  (declare-function on-make-mode-init! (v-home%> "config/compiles"))
  (autoload 'on-compile-init! (v-home%> "config/compiles"))
  (autoload 'on-grep-init! (v-home%> "config/compiles"))
  (autoload 'on-make-mode-init! (v-home%> "config/compiles"))
  (with-eval-after-load 'compile
    (make-thread* #'on-compile-init!))
  (with-eval-after-load 'grep
    (make-thread* #'on-grep-init!))
  (with-eval-after-load 'make-mode
    (make-thread* #'on-make-mode-init!))
  ;; `direds'
  (declare-function on-dired-init! (v-home%> "config/direds"))
  (declare-function on-dired-aux-init! (v-home%> "config/direds"))
  (declare-function on-arc-mode-init! (v-home%> "config/direds"))
  (autoload 'on-dired-init! (v-home%> "config/direds"))
  (autoload 'on-dired-aux-init! (v-home%> "config/direds"))
  (autoload 'on-arc-mode-init! (v-home%> "config/direds"))
  (with-eval-after-load 'dired
    (make-thread* #'on-dired-init!))
  (with-eval-after-load 'dired-aux
    (make-thread* #'on-dired-aux-init!))
  (with-eval-after-load 'arc-mode
    (make-thread* #'on-arc-mode-init!))
  ;; `docs'
  (when-platform% windows-nt
    (declare-function on-doc-view-init! (v-home%> "config/docs"))
    (autoload 'on-doc-view-init! (v-home%> "config/docs"))
    (with-eval-after-load 'doc-view
      (make-thread* #'on-doc-view-init!)))
  ;; `eglots'
  (when-feature% eglot
    (declare-function on-eglot-init! (v-home%> "config/eglots"))
    (autoload 'on-eglot-init! (v-home%> "config/eglots"))
    (with-eval-after-load 'eglot
      (make-thread* #'on-eglot-init!)))
  ;; `elisps'
  (declare-function on-elisp-init! (v-home%> "config/elisps"))
  (autoload 'on-elisp-init! (v-home%> "config/elisps"))
  (if-version%
      <= 25.0
      (with-eval-after-load 'elisp-mode
        (make-thread* #'on-elisp-init!))
    (with-eval-after-load 'lisp-mode
      (make-thread* #'on-elisp-init!)))
  ;; `ewws'
  (when-feature% eww
    (declare-function on-eww-init! (v-home%> "config/ewws"))
    (declare-function lookup-web (v-home%> "config/ewws") nil t)
    (autoload 'on-eww-init! (v-home%> "config/ewws"))
    (autoload 'lookup-web (v-home%> "config/ewws") nil t)
    (autoload 'toggle-browser! (v-home%> "config/ewws") nil t)
    (with-eval-after-load 'eww
      (make-thread* #'on-eww-init!)))
  ;; `helps'
  (declare-function on-help-init! (v-home%> "config/helps"))
  (autoload 'on-help-init! (v-home%> "config/helps"))
  (with-eval-after-load 'help-mode
    (make-thread* #'on-help-init!))
  ;; `hippies'
  (declare-function on-hippie-init! (v-home%> "config/hippies"))
  (autoload 'on-hippie-init! (v-home%> "config/hippies"))
  (with-eval-after-load 'hippie-exp
    (make-thread* #'on-hippie-init!))
  ;; `orgs'
  (declare-function on-org-init! (v-home%> "config/orgs"))
  (autoload 'on-org-init! (v-home%> "config/orgs"))
  (with-eval-after-load 'org
    (make-thread* #'on-org-init!))
  (when-version% >= 23
    (push! (cons "\\.org\\'" 'org-mode) auto-mode-alist))
  ;; `projects'
  (when-feature% project
    (declare-function on-project-init! (v-home%> "config/projects"))
    (autoload 'on-project-init! (v-home%> "config/projects"))
    (autoload 'project*-root-dirs (v-home%> "config/projects"))
    (with-eval-after-load 'project
      (make-thread* #'on-project-init!)))
  ;; `progs'
  (declare-function on-progs-init! (v-home%> "config/progs"))
  (autoload 'on-progs-init! (v-home%> "config/progs"))
  (with-eval-after-load 'prog-mode
    (make-thread* #'on-progs-init!))
  ;; `pythons'
  (declare-function on-python-init! (v-home%> "config/pythons"))
  (autoload 'on-python-init! (v-home%> "config/pythons"))
  (autoload 'python*-program (v-home%> "config/pythons"))
  (autoload 'python*-venv (v-home%> "config/pythons"))
  (autoload 'python*-venv-make! (v-home%> "config/pythons") nil t)
  (with-eval-after-load 'python
    (make-thread* #'on-python-init!))
  ;; `sqls'
  (declare-function on-sql-init! (v-home%> "config/sqls"))
  (autoload 'on-sql-init! (v-home%> "config/sqls"))
  (with-eval-after-load 'sql
    (make-thread* #'on-sql-init!))
  ;; `terms'
  (declare-function on-eshell-init! (v-home%> "config/eshells"))
  (declare-function on-ielm-init! (v-home%> "config/elisps"))
  (declare-function on-term-init! (v-home%> "config/terms"))
  (autoload 'on-eshell-init! (v-home%> "config/eshells"))
  (autoload 'on-ielm-init! (v-home%> "config/elisps"))
  (autoload 'on-term-init! (v-home%> "config/terms"))
  (autoload 'term*-unify-shell-prompt (v-home%> "config/terms") nil t)
  (with-eval-after-load 'term
    (make-thread* #'on-term-init!))
  (with-eval-after-load 'eshell
    (make-thread* #'on-eshell-init!))
  (with-eval-after-load 'ielm
    (make-thread* #'on-ielm-init!))
  ;; `tramps'
  (declare-function on-tramp-init! (v-home%> "config/tramps"))
  (autoload 'on-tramp-init! (v-home%> "config/tramps"))
  (with-eval-after-load 'tramp
    (make-thread* #'on-tramp-init!))
  ;; `transients'
  (when-feature% transient
    (declare-function on-transient-init! (v-home%> "config/transients"))
    (autoload 'on-transient-init! (v-home%> "config/transients"))
    (with-eval-after-load 'transient
      (make-thread* #'on-transient-init!))
    (unless-graphic%
      (when-version% > 23 (transient-mark-mode t))))
  ;; `xrefs'
  (declare-function on-xref-init! (v-home%> "config/xrefs"))
  (declare-function on-etags-init! (v-home%> "config/xrefs"))
  (autoload 'on-xref-init! (v-home%> "config/xrefs"))
  (autoload 'on-etags-init! (v-home%> "config/xrefs"))
  (autoload 'xref*-read-only-dirs (v-home%> "config/xrefs"))
  (with-eval-after-load 'xref
    (make-thread* #'on-xref-init!))
  (with-eval-after-load 'etags
    (make-thread* #'on-etags-init!)))

(defun define-global-key! ()
  ;; `compiles'
  (define-global-key% "pc" #'compile)
  ;; `ewws'
  (when-feature% eww
    (define-global-key% (kbd "M-s w") #'lookup-web)
    (when-fn% eww-list-bookmarks eww
      (define-global-key% (kbd "M-s M-b") #'eww-list-bookmarks)))
  ;; `hippies'
  (define-global-key% (kbd "M-/") #'hippie-expand)
  ;; `isearchs'
  (define-global-key% "" #'isearch*-forward)
  (define-global-key% "" #'isearch*-backward)
  (define-global-key% (kbd "M-s .") #'isearch*-forward-symbol)
  ;; `marks' kill
  (define-global-key% (kbd "C-x M-d") #'kill-word@)
  (define-global-key% (kbd "C-x M-e") #'kill-sexp@)
  (define-global-key% (kbd "C-x M-s") #'kill-string@)
  (define-global-key% (kbd "C-x M-l") #'kill-whole-line)
  ;; `marks' mark
  (define-global-key% (kbd "C-c C-M-@") #'mark-sexp@)
  (define-global-key% (kbd "C-c M-@") #'mark-word@)
  (define-global-key% (kbd "C-c M-f") #'mark-filename@)
  (define-global-key% (kbd "C-c M-h") #'mark-defun@)
  (define-global-key% (kbd "C-c M-l") #'mark-line@)
  (define-global-key% (kbd "C-c M-s") #'mark-string@)
  (define-global-key% (kbd "C-M-@") #'mark-sexp)
  (define-global-key% (kbd "C-M-SPC") #'mark-sexp)
  (define-global-key% (kbd "C-M-h") #'mark-defun)
  (define-global-key% (kbd "M-@") #'mark-word)
  ;; `projects'
  (when-feature% project
    (when-fn% project-find-file project
      (define-global-key% "pf" #'project-find-file))
    (when-fn% project-find-regexp project
      (define-global-key% "pg" #'project-find-regexp)))
  ;; `vcs'
  (when-feature-vc%
    (define-global-key% "vd" #'vc*-dir))
  ;; `winmoves'
  (unless-fn% recenter-top-bottom nil
    (define-global-key% "" #'recenter-top-bottom))
  (define-global-key% "wl" #'windmove-left)
  (define-global-key% "wr" #'windmove-right)
  (define-global-key% "wu" #'windmove-up)
  (define-global-key% "wd" #'windmove-down))

;; end of `define-global-key!'

(defun self-epilogue-init! ()
  (when-graphic%
    (when-platform% darwin
      (when-fn% mac-process-deferred-apple-events nil
        (mac-process-deferred-apple-events))
      (when-fn% mac-mouse-wheel-mode mac-win
        (mac-mouse-wheel-mode 1))))
  (when (*self-paths* :get :epilogue)
    (condition-case err
        (prog1 t
          (compile! (compile-unit* (*self-paths* :get :epilogue))))
      (error (prog1 nil (message "self-epilogue-init!: %s" err))))))

;; end of `self-epilogue-init!'

;; after-init
(defun on-autoloads! ()
  "Autoload after init."
  (set 'after-init-hook nil)
  (self-graphic-init!)
  (when-fn% self-shell-read! nil (self-shell-read!))
  (when-fn% self-socks-init! nil (self-socks-init!))
  (setq% history-length (emacs-arch%))
  (setq% message-log-max 512)
  ;; preferred coding system
  (prefer-coding-system 'utf-8)
  ;; define modes
  (define-prologue-mode)
  (declare-prologue-mode!)
  (define-compile-only-mode)
  (declare-compile-only-mode!)
  (define-after-load-mode)
  (declare-after-load-mode!)
  (define-global-key!)
  (when-fn% self-edit-init! nil (self-edit-init!))
  (when-fn% self-key-init! nil (self-key-init!))
  (when-fn% self-glyph-init! nil (make-thread* #'self-glyph-init!))
  (when-fn% self-module-init! nil
    (condition-case err
        (self-module-init!)
      (error (prog1 nil (message "self-module-init!: %s" err)))))
  (when-fn% self-desktop-read! nil
    (condition-case err
        (self-desktop-read!)
      (error (prog1 nil (message "self-desktop-read!: %s" err)))))
  ;; clean dumb hooks
  (set 'after-init-hook nil)
  ;; `load-path' versioned dirs
  (push! (v-home% "config/") load-path)
  (push! (v-home% "private/") load-path)
  (make-thread* #'self-epilogue-init! (unless-interactive% t)))

;; end of autoloads.el
