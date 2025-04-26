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
   (prog1 (compile-unit% (emacs-home* "config/cls.el") t)
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
     (autoload 'take-while (v-home%> "config/cls")))
   (unless-graphic%
     (prog1 (compile-unit% (emacs-home* "config/clipboard.el") t)
       (declare-function on-clipboard-init! (v-home%> "config/clipboard"))
       (autoload 'on-clipboard-init! (v-home%> "config/clipboard"))))
   (prog1 (compile-unit% (emacs-home* "config/ed.el") t)
     (autoload 'delete-line* (v-home%> "config/ed"))
     (autoload 'file-in-dirs-p (v-home%> "config/ed"))
     (autoload 'newline* (v-home%> "config/ed") nil t)
     (autoload 'open-next-line (v-home%> "config/ed") nil t)
     (autoload 'open-previous-line (v-home%> "config/ed") nil t)
     (autoload 'read-string-prompt (v-home%> "config/ed"))
     (autoload 'select-region-prompt (v-home%> "config/ed"))
     (autoload 'vstrncmp (v-home%> "config/ed")))
   (prog1 (compile-unit% (emacs-home* "config/marks.el") t)
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
     (autoload 'mark-word@ (v-home%> "config/marks") nil t))
   (prog1 (compile-unit% (emacs-home* "config/ssh.el") t)
     (autoload 'ssh-remote-p (v-home%> "config/ssh"))
     (autoload 'ssh-remote->ids (v-home%> "config/ssh"))
     (autoload 'ssh-remote->user@host (v-home%> "config/ssh"))
     (autoload 'ssh-remote-command (v-home%> "config/ssh")))
   (prog1 (compile-unit% (emacs-home* "config/tags.el") t)
     (autoload 'tags-spec->* (v-home%> "config/tags"))
     (autoload 'make-c-tags (v-home%> "config/tags"))
     (autoload 'make-dir-ctags (v-home%> "config/tags"))
     (autoload 'make-dir-tags (v-home%> "config/tags") nil t)
     (autoload 'mount-tags (v-home%> "config/tags") nil t)
     (autoload 'unmount-tags (v-home%> "config/tags") nil t))
   (prog1 (compile-unit% (emacs-home* "config/thingatpts.el") t)
     (declare-function on-thingatpt-init! (v-home%> "config/thingatpts"))
     (autoload 'on-thingatpt-init! (v-home%> "config/thingatpts")))))

(defun load-compiling-modes! ()
  "Load compiling modes."
  (compile!
   (compile-unit% (emacs-home* "config/cc.el") t)
   (prog1 (compile-unit% (emacs-home* "config/chez.el") t)
     (autoload 'chez-mode (v-home%> "config/chez") "Toggle chez mode." t)
     (autoload 'run-chez (v-home%> "config/chez") "Toggle chez process." t))
   (compile-unit% (emacs-home* "config/compiles.el") t)
   (prog1 (compile-unit% (emacs-home* "config/cscope.el") t)
     (autoload 'cscope (v-home%> "config/cscope") "Run cscope." t)
     (autoload 'run-cscope (v-home%> "config/cscope") "Run cscope REPL." t))
   (prog1 (compile-unit% (emacs-home* "config/dict.el") t)
     (autoload 'lookup-dict (v-home%> "config/dict") "Lookup dict." t)
     (define-key (current-global-map) (kbd% "M-s d") 'lookup-dict))
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
   (prog1 (compile-unit% (emacs-home* "config/jshell.el") t)
     (autoload 'jshell-mode (v-home%> "config/jshell") "Toggle jshell mode." t)
     (autoload 'run-jshell (v-home%> "config/jshell")
       "Toggle jshell process." t)
     (push! `("\\.jsh\\'" . java-mode) auto-mode-alist))
   ;; `scheme': `gambit-mode' abandoned
   ;; (prog1 (compile-unit% (emacs-home* "config/gambit.el") t)
   ;;   (autoload 'gambit-mode (v-home%> "config/gambit") "Toggle gambit." t))
   (when-platform% windows-nt
     (prog1 (compile-unit% (emacs-home* "config/gud-cdb.el") t)
       (autoload 'gud-cdb (v-home%> "config/gud-cdb") "Run cdb." t)))
   (prog1 (compile-unit% (emacs-home* "config/gud-lldb.el") t)
     (autoload 'gud-lldb (v-home%> "config/gud-lldb") "Run lldb." t))
   ;; TODO: recast
   ;; (compile-unit% (emacs-home* "config/mixal.el") t)
   (prog1 (compile-unit% (emacs-home* "config/node.el") t)
     (autoload 'node-mode (v-home%> "config/node") "Toggle node mode." t)
     (autoload 'run-node (v-home%> "config/node") "Toggle node process." t))
   (compile-unit% (emacs-home* "config/orgs.el") t)
   (when-feature% project
     (compile-unit% (emacs-home* "config/projects.el") t))
   (prog1 (compile-unit% (emacs-home* "config/progs.el") t)
     (declare-function on-progs-init! (v-home%> "config/progs"))
     (autoload 'on-progs-init! (v-home%> "config/progs")))
   (compile-unit% (emacs-home* "config/pythons.el") t)
   (prog1 (compile-unit% (emacs-home* "config/scratch.el") t)
     (autoload 'scratch (v-home%> "config/scratch") "Scratch" t))
   (compile-unit% (emacs-home* "config/sqls.el") t)
   (prog1 (compile-unit% (emacs-home* "config/sudoku.el") t)
     (autoload 'sudoku (v-home%> "config/sudoku") "Play sudoku." t))
   (compile-unit% (emacs-home* "config/terms.el") t)
   (prog1 (compile-unit% (emacs-home* "config/trans.el") t)
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
     (autoload 'roman->arabic (v-home%> "config/trans") nil t))
   (when-feature% transient
     (compile-unit% (emacs-home* "config/transients.el") t))
   (when-feature-treesit%
     (prog1 (compile-unit% (emacs-home* "config/treesits.el") t)
       (autoload 'toggle-treesit! (v-home%> "config/treesits") nil t
         "Toggle treesit.")))
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
   ;; on `js'
   ;; (compile-unit% (emacs-home* "config/on-js-autoload.el"))
   ;; on `mixal'
   ;; (compile-unit% (emacs-home* "config/on-mixal-autoload.el"))
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
   ;; on `tramps'
   (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
   ;; on `xrefs'
   (compile-unit% (emacs-home* "config/on-xref-autoload.el"))
   ;; on `vcs'
   (when-feature-vc%
     (compile-unit% (emacs-home* "config/on-vcs-autoload.el")))
   ;; `windows'
   (compile-unit% (emacs-home* "config/windows.el"))
   ) ;; end of compile!
  (with-eval-after-load 'thingatpt
    (make-thread* #'on-thingatpt-init!))
  (make-thread* #'on-clipboard-init!)
  (make-thread* #'on-progs-init!)
  )
;; end of `load-conditional-modes!'

(defun define-global-keys! ()
  (let ((keymap (current-global-map)))
    ;; `isearchs'
    (declare-function isearch*-forward (v-home%> "config/isearchs"))
    (declare-function isearch*-backward (v-home%> "config/isearchs"))
    (declare-function isearch*-forward-symbol (v-home%> "config/isearchs"))
    (autoload 'isearch*-forward (v-home%> "config/isearchs"))
    (autoload 'isearch*-backward (v-home%> "config/isearchs"))
    (autoload 'isearch*-forward-symbol (v-home%> "config/isearchs"))
    (define-key keymap "" #'isearch*-forward)
    (define-key keymap "" #'isearch*-backward)
    (define-key keymap (kbd "M-s .") #'isearch*-forward-symbol)
    ;; kill
    (define-key keymap (kbd% "C-x M-d") #'kill-word@)
    (define-key keymap (kbd% "C-x M-e") #'kill-sexp@)
    (define-key keymap (kbd% "C-x M-s") #'kill-string@)
    (define-key keymap (kbd% "C-x M-l") #'kill-whole-line)
    ;; mark
    (define-key keymap (kbd% "C-c C-M-@") #'mark-sexp@)
    (define-key keymap (kbd% "C-c M-@") #'mark-word@)
    (define-key keymap (kbd% "C-c M-f") #'mark-filename@)
    (define-key keymap (kbd% "C-c M-h") #'mark-defun@)
    (define-key keymap (kbd% "C-c M-l") #'mark-line@)
    (define-key keymap (kbd% "C-c M-s") #'mark-string@)
    (define-key keymap (kbd% "C-M-@") #'mark-sexp)
    (define-key keymap (kbd% "C-M-SPC") #'mark-sexp)
    (define-key keymap (kbd% "C-M-h") #'mark-defun)
    (define-key keymap (kbd% "M-@") #'mark-word)))

;; end of `define-global-keys!'

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
  (load-prologue-modes!)
  (load-compiling-modes!)
  (load-conditional-modes!)
  (define-global-keys!)
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
