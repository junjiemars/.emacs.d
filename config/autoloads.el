;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; autoloads.el
;;;;


;;; semantic, require Emacs-24.4+
(defmacro-if-feature% semantic)

;;; default web browser: eww, requires Emacs-24.4+
(defmacro-if-feature% eww)

(defalias '*org-babel-schemes*
  (lexical-let% ((i '()))
    (lambda (&optional op key val)
      (cond ((eq :get op) (plist-get i key))
            ((eq :put op) (setq i (plist-put i key val)))
            (t i))))
  "The available Scheme's implementations for `ob'.")

(defmacro autoload* (symbol file &optional docstring interactive type)
  "Force autoload SYMBOL, like `autoload' does."
  `(progn
     (fset ,symbol nil)
     (setplist ,symbol nil)
     (fset ,symbol
           `(autoload ,,file ,,docstring ,,interactive ,,type))))


(defun load-autoloaded-modes! ()
  "Load autoloaded modes."
  (compile!

    (compile-unit% (emacs-home* "config/financial.el") t)
    (compile-unit% (emacs-home* "config/go.el") t)
    (compile-unit% (emacs-home* "config/guds.el") t)
    (compile-unit% (emacs-home* "config/scratch.el"))
    (compile-unit% (emacs-home* "config/tags.el"))
    (compile-unit% (emacs-home* "config/on-cc-autoload.el"))
    (compile-unit% (emacs-home* "config/on-compile-autoload.el"))
    (compile-unit% (emacs-home* "config/on-dired-autoload.el"))
    (compile-unit% (emacs-home* "config/on-edit-autoload.el"))
    (compile-unit% (emacs-home* "config/on-trans-autoload.el"))
    (compile-unit% (emacs-home* "config/on-eshell-autoload.el"))
    (compile-unit% (emacs-home* "config/on-hippie-autoload.el"))
    (compile-unit% (emacs-home* "config/on-indent-autoload.el"))
    (compile-unit% (emacs-home* "config/on-isearch-autoload.el"))
    (compile-unit% (emacs-home* "config/on-js-autoload.el"))
    (compile-unit% (emacs-home* "config/on-lisp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-net-autoload.el"))
    (compile-unit% (emacs-home* "config/on-org-autoload.el"))
    (compile-unit% (emacs-home* "config/on-pp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-sql-autoload.el"))
    (compile-unit% (emacs-home* "config/on-term-autoload.el"))
    (compile-unit% (emacs-home* "config/on-tramp-autoload.el"))
    (compile-unit% (emacs-home* "config/on-window-autoload.el"))))
 ;; end of `load-autoloaded-modes!'

(defun load-conditional-modes! ()
  "Load conditional modes."
  (compile!
    ;; Lookup dictionary
    (progn
      (autoload 'lookup-dict (v-home%> "config/dict")
        "Lookup WORD in DICT then show the result in the echo area." t)
      (compile-unit% (emacs-home* "config/dict.el")))

    ;; Glyph
    (when-font%
      (compile-unit% (emacs-home* "config/on-glyph-autoload.el")))

    ;; `semantic-mode'
    (if-feature-semantic%
        (progn
          (autoload 'set-semantic-cc-env!
            (v-home%> "config/on-semantic-autoload"))
          (compile-unit% (emacs-home* "config/on-semantic-autoload.el"))))

    ;; `eww-mode'
    (if-feature-eww%
        (compile-unit% (emacs-home* "config/on-eww-autoload.el")))

    ;; `doc-view-mode'
    (when-platform% 'windows-nt
      (when% (or (executable-find% "gswin64c")
                 (executable-find% "gswin32c")
                 (executable-find% "mutool"))
        (compile-unit% (emacs-home* "config/on-docview-autoload.el"))))

    ;; Debugger `gud-cdb'
    (when-platform% 'windows-nt
      (when% (executable-find% "cdb")
        (prog1
            (compile-unit% (emacs-home* "config/gud-cdb.el") t)
          (autoload 'gud-cdb (v-home%> "config/gud-cdb")
            "Run lldb on program FILE in buffer *gud-FILE*." t))))

    ;; Debugger `gud-lldb'
    (when% (executable-find% "lldb")
      (prog1
          (compile-unit% (emacs-home* "config/gud-lldb.el") t)
        (autoload 'gud-lldb (v-home%> "config/gud-lldb")
          "Run lldb on program FILE in buffer *gud-FILE*." t)))

    ;; Jshell
    (when% (executable-find% "jshell"
                             (lambda (jshell)
                               (let ((x (shell-command* jshell "--version")))
                                 (zerop (car x)))))
      (prog1
          (compile-unit% (emacs-home* "config/jshell.el") t)
        (autoload 'jshell-mode (v-home%> "config/jshell")
          "Toggle Jshell's mode." t)
        (autoload 'run-jshell (v-home%> "config/jshell")
          "Toggle jshell process in buffer `*jshell*'." t)))

    ;; Node
    (when% (or (file-exists-p "~/.nvm/nvm.sh")
               (executable-find% "node"
                                 (lambda (node)
                                   (let ((x (shell-command* "echo"
                                              "'1+2+3'|" node "-p")))
                                     (zerop (car x))))))
      (prog1
          (compile-unit% (emacs-home* "config/node.el") t)
        (autoload 'node-mode (v-home%> "config/node")
          "Toggle Node's mode." t)
        (autoload 'run-node (v-home%> "config/node")
          "Toggle node process in buffer `*node*'." t)))

    ;; Python
    (when% (executable-find%
            "python"
            (lambda (python)
              (let ((x (shell-command* "python -c'print(1+2)'")))
                (and (zerop (car x))
                     (string= "3" (string-trim> (cdr x)))))))
      (compile-unit% (emacs-home* "config/on-python-autoload.el")))

    ;; Scheme `gambit-mode'
    (when% (executable-find% "gsc-script"
                             (lambda (gsc)
                               (let ((x (shell-command* gsc
                                          "-e \"(system-type)\"")))
                                 (zerop (car x)))))
      (prog1
          (compile-unit% (emacs-home* "config/gambit.el") t)
        (*org-babel-schemes* :put 'gambit "gsc-script")
        (autoload 'gambit-mode (v-home%> "config/gambit")
          "Toggle Gambit's mode." t)
        (autoload* 'run-gambit (v-home%> "config/gambit")
                   "Toggle gambit process in buffer `*gambit*'." t)))

    ;; Scheme `chez-mode'
    (when% (executable-find% "scheme"
                             (lambda (chez)
                               (let ((x (shell-command* "echo"
                                          "'(scheme-version)'|" chez "-q")))
                                 (zerop (car x)))))
      (prog1
          (compile-unit% (emacs-home* "config/chez.el") t)
        (*org-babel-schemes* :put 'chez "scheme")
        (autoload 'chez-mode (v-home%> "config/chez")
          "Toggle Chez's mode." t)
        (autoload* 'run-chez (v-home%> "config/chez")
                   "Toggle chez process in buffer `*chez*'." t)))

    ;; Org `ob' for Scheme
    (prog1
        (compile-unit% (emacs-home* "config/ob-schemes.el") t)
      (when (*org-babel-schemes*)
        (autoload* 'org-babel-execute:scheme*
                   (v-home%> "config/ob-schemes")
                   "Autoload `org-babel-execute:scheme*'." t)
        (fset 'org-babel-execute:scheme 'org-babel-execute:scheme*)))

    ;; Sudoku
    (prog1
        (compile-unit% (emacs-home* "config/sudoku.el") t)
      (autoload* 'sudoku (v-home%> "config/sudoku")
                 "Play sudoku." t))

    ;; Vlang
    (when% (executable-find% "v"
                             (lambda (v)
                               (let ((x (shell-command* "v"
                                          "version")))
                                 (zerop (car x)))))
      (prog1
          (compile-unit% (emacs-home* "config/vlang.el") t)
        (autoload 'vlang-mode (v-home%> "config/vlang")
          "Toggle Vlang's mode." t)))


    ) ;; end of compile!

  ;; *scratch*
  (when (get-buffer "*scratch*")
    (with-current-buffer "*scratch*"
      (lisp-interaction-mode)))

  ;; `view-mode'
  (setq view-read-only t)

  ;; enable column number mode
  (setq% column-number-mode t 'simple)

  ) ;; end of `load-conditional-modes!'


(defun set-global-keys! ()
  "Set global keys."

  ;; Lookup dictionary
  (define-key% (current-global-map) (kbd "M-s d") 'lookup-dict)
  (define-key% (current-global-map) (kbd "C-c f d") 'lookup-dict)

  ;; Open file or url at point
  (when-fn% 'find-file-at-point 'ffap
    (define-key% (current-global-map) (kbd "C-c f f") #'find-file-at-point))

  ;; Shows a list of buffers
  (define-key% (current-global-map) (kbd "C-x C-b") #'ibuffer)

  ;; Interactive query replace key bindings.
  (define-key% (current-global-map) (kbd "M-%") #'query-replace-regexp)
  (define-key% (current-global-map) (kbd "C-M-%") #'query-replace)

  ;; Register
  ;; `C-x r g' and `C-x r i' are all bound to insert-register
  ;; let `C-x r g' do `string-insert-rectangle'
  (define-key% (current-global-map) (kbd "C-x r g") #'string-insert-rectangle)
  (define-key% (current-global-map) (kbd "C-x r v") #'view-register)

  ;; Buffer
  (unless-fn% 'revert-buffer-quick nil
    (define-key% (current-global-map) (kbd "C-x x g") #'revert-buffer))
  (when-fn% 'linum-mode 'linum
    (define-key% (current-global-map) (kbd "C-c b l") #'linum-mode))

  ;; Line
  (when-fn% 'electric-newline-and-maybe-indent 'electric
    ;; Default behaviour of RET
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00490.html
    ;; electric-indent-mode: abolition of `newline' function is not
    ;; the Right Thing
    ;; https://lists.gnu.org/archive/html/emacs-devel/2013-10/msg00407.html
    (define-key% (current-global-map) (kbd "RET")
      #'electric-newline-and-maybe-indent)
    (define-key% (current-global-map) (kbd "C-j") #'newline))

  ;; Sorting
  (define-key% (current-global-map) (kbd "C-c s f") #'sort-fields)
  (define-key% (current-global-map) (kbd "C-c s n") #'sort-numeric-fields)
  (define-key% (current-global-map) (kbd "C-c s x") #'sort-regexp-fields)
  (define-key% (current-global-map) (kbd "C-c s l") #'sort-lines)
  (define-key% (current-global-map) (kbd "C-c s r") #'reverse-region)
  (define-key% (current-global-map) (kbd "C-c s d") #'delete-duplicate-lines)

  ;; View
  (define-key% (current-global-map) (kbd "C-x 4 v") #'view-file-other-window)
  (define-key% (current-global-map) (kbd "C-x 5 v") #'view-file-other-frame)
  (define-key (current-global-map) (kbd "C-x C-v") #'view-file)


  ) ;; end of set-global-keys!


;; after-init
(defun on-autoloads! ()
  (make-thread*
   (lambda ()
     (compile! (compile-unit% (emacs-home* "config/sockets.el")))
     (when-package%
       (compile! (compile-unit% (emacs-home* "config/module.el"))))
     (compile! (compile-unit% (emacs-home* "config/on-module.el")))
     (package-spec-:allowed-p (apply #'compile! (*package-compile-units*)))
     (load-autoloaded-modes!)
     (load-conditional-modes!)
     (set-global-keys!)
     (compile! (compile-unit% (emacs-home* "config/safe.el")))
     (when (*self-paths* :get :epilogue)
       (compile! (compile-unit* (*self-paths* :get :epilogue))))
     (ido-mode t)
     (when-fn% 'self-desktop-read! nil (self-desktop-read!)))
   t "on-autoloads!"))


;;; autoload when interactive or not
(if noninteractive
    (on-autoloads!)
  (add-hook 'after-init-hook #'on-autoloads! t))


;; end of autoloads.el
