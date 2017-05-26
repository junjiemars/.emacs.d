(defmacro comment (&rest body)
  "Ignores body, yields nil."
  nil)

(defvar loading-start-time
  (current-time) "The start time at loading init.el")

(defvar emacs-home
  (if (boundp 'user-emacs-directory)
      user-emacs-directory
    "~/.emacs.d/")
  "The user's emacs home directory")

(defvar v-dir (concat (if (display-graphic-p) "g_" "t_")
                      emacs-version)
  "Versionized dir based on grahpic/terminal mode and Emacs's version")

(defmacro make-vdir (&optional subdir)
  "Make the versionized SUBDIR under emacs-home and returns it. "
  (let ((_vdir_ (concat emacs-home subdir v-dir "/")))
    `(progn
       (when (not (file-exists-p ,_vdir_))
         (make-directory ,_vdir_ t))
       ,_vdir_)))

(defmacro compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp FILES under the SUBDIR."
  `(let ((d (concat emacs-home ,subdir))
         (v (make-vdir ,subdir)))
     (dolist (f ,files)
       (let ((from (concat d f)))
         (if (file-exists-p from)
             (let ((c (replace-regexp-in-string "\.el$" "\.elc" f)))
               (when (or (not (file-exists-p (concat v c)))
                         (file-newer-than-file-p from (concat v c)))
                 (copy-file from (concat v f) t)
                 (byte-compile-file (concat v f)))
               (load (concat v c)))
           (message "#Skip compile and load %s.done" from))))))


(defmacro package-supported-p (&rest body)
  "Run BODY code if supports package.

\(fn BODY...)"
  (declare (indent 0))
  (when (>= emacs-major-version 24)
    `(progn ,@body)))

(defmacro platform-supported-if (os then &rest else)
  "If (eq system-type OS) yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (eq system-type OS) yields nil, and there are no ELSE’s, the value is nil.

\(fn OS THEN ELSE...)"
  (declare (indent 2))
  (if (eq system-type os)
      `,then
    `(progn ,@else)))

(defmacro platform-supported-when (os &rest body)
  "Run BODY code if on specified OS platform.

\(fn OS BODY...)"
  (declare (indent 1))
  `(platform-supported-if ,os (progn ,@body)))

(defmacro platform-supported-unless (os &rest body)
  "Run BODY code unless on specified OS platform.

\(fn OS BODY...)"
  (declare (indent 1))
  `(platform-supported-if ,os nil ,@body))

(defmacro version-supported-p (cond version)
  "Return true if (COND VERSION EMACS-VERSION) yields non-nil, else nil.

\(fn COND VERSION)"
  `(funcall ,cond ,version (string-to-number emacs-version)))

(defmacro version-supported-if (cond version then &rest else)
  "If (COND VERSION EMACS-VERSION) yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (COND VERSION EMACS-VERSION) yields nil, and there are no ELSE’s, the value is nil.

\(fn COND VERSION THEN ELSE...)"
  (declare (indent 3))
  (if (version-supported-p `,cond `,version)
      `,then
    `(progn ,@else)))

(defmacro version-supported-when (cond version &rest body)
  "If (COND VERSION EMACS-VERSION) yields non-nil, do BODY, else return nil.
When (COND VERSION EMACS-VERSION) yields non-nil, eval BODY forms sequentially and return value of last one, or nil if there are none.

\(fn COND VERSION BODY...)"
  (declare (indent 2))
  `(version-supported-if ,cond ,version (progn ,@body)))

(defmacro graphic-supported-if (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If in terminal mode, and there are no ELSE’s, the value is nil.

\(fn THEN ELSE...)"
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn ,@else)))

(defmacro graphic-supported-p (&rest body)
  "Run BODY code if in graphic mode.

\(fn BODY...)"
  (declare (indent 0))
  `(graphic-supported-if (progn ,@body)))

(defmacro terminal-supported-p (&rest body)
  "Run BODY code if in terminal mode.

\(fn BODY...)"
  (declare (indent 0))
  `(graphic-supported-if nil ,@body))

(defmacro bin-exists-p (b)
  "Returns true if B exists in env.

\(fn BIN-NAME)"
  (platform-supported-if windows-nt
      `(zerop (shell-command (concat "where " ,b " >nul 2>&1")))
    `(zerop (shell-command (concat "hash " ,b " &>/dev/null")))))

(defmacro shell-command-to-string-no-newline (c)
  `(replace-regexp-in-string "\n$" "" (shell-command-to-string ,c)))

(defmacro bin-path (b)
  "Returns the path of B in env.

\(fn BIN-NAME)"
  (platform-supported-if windows-nt
      `(shell-command-to-string-no-newline (concat "where " ,b))
    `(shell-command-to-string-no-newline (concat "type -P " ,b))))

(defmacro windows-nt-path (p)
  "Return the path that windows-nt can recoganized."
  `(replace-regexp-in-string "\\\\" "/" ,p))

(defmacro safe-call (fn &rest args)
  "Call FN with ARGS if FN has been bound.

\(fn FN-NAME ARGS...)"
  (declare (indent 1))
  (when (fboundp fn)
    `(,fn ,@args)))

(defmacro safe-do-if (fn then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If FN is not bounded yields nil, and there are no ELSE’s, the value is nil.

\(fn FN THEN ELSE...)"
  (declare (indent 2))
  (if (or (fboundp fn))
      `,then
    `(progn ,@else)))

(defmacro safe-do-when (fn &rest body)
  "Do BODY when FN is bound.

\(fn FN BODY...)"
  (declare (indent 1))
  `(safe-do-if ,fn (progn ,@body)))

(defmacro safe-do-when* (fn &rest body)
  "Do BODY when FN is local bound.

\(fn FN-LOCAL BODY...)"
  (declare (indent 1))
  `(when (fboundp ,fn) (progn ,@body)))

(defmacro safe-do-when! (x &rest body)
  "Do BODY when X is bound.

\(fn X BODY...)"
  (declare (indent 1))
  (when (boundp x)
    `(progn ,@body)))

(defmacro safe-do-when!* (x &rest body)
  "Do BODY when X is local bound.

\(fn X-LOCAL BODY...)"
  (declare (indent 1))
  `(when (boundp ,x) ,@body))

(defmacro safe-setq (x val)
  "Set X when variable X is bound.

\(fn X VALUE)"
  (when (boundp x)
    `(setq ,x ,val)))

(defmacro safe-setq* (x val)
  "Set X when variable X is local bound.

\(fn X-LOCAL VALUE)"
  `(when (boundp (quote ,x))
     (setq ,x ,val)))

(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))

(defmacro start-socks (&optional port server version)
  "Switch on url-gateway to socks"
  `(version-supported-when < 22
     (require 'url)
     (setq url-gateway-method 'socks)
     (setq-default socks-server
                   (list "Default server"
                         (if ,server ,server "127.0.0.1")
                         (if ,port ,port 32000)
                         (if ,version ,version 5)))))

(defmacro stop-socks (&optional method)
  "Switch off url-gateway to native."
  `(version-supported-when < 22
     (require 'url)
     (setq url-gateway-method
           (if ,method  ,method 'native))))

(defmacro clean-compiled-files ()
  "Clean all compiled files, need restart Emacs."
  `(dolist (d (list (make-vdir "config/")
                    (make-vdir "private/")))
     (dolist (f (directory-files d nil "\\.elc$"))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))

(defmacro clean-saved-user-files ()
  "Clean saved desktop, need restart Emacs."
  `(let ((dirs (list (make-vdir ".auto-save/")
                     (make-vdir ".desktop/")
                     (make-vdir ".bookmarks/")
                     (make-vdir ".ido/")
                     (make-vdir ".minibuffer/")
                     (make-vdir ".recentf/")
                     (make-vdir ".places/")
                     (make-vdir ".smex/"))))
     (dolist (d dirs)
       (when (file-exists-p d)
         (dolist (f (directory-files d nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
           (message "#Clean saved user file: %s" (concat d f))
           (delete-file (concat d f)))))))

(defmacro reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  `(progn
     (clean-compiled-files)
     (clean-saved-user-files)
     (kill-emacs 0)))

(defmacro save-expr-to-file (expr filename)
  "Save `expr' to a file"
  `(save-excursion
     (let ((expr-buffer (find-file-noselect ,filename)))
       (set-buffer expr-buffer)
       (erase-buffer)
       (print ,expr expr-buffer)
       (save-buffer)
       (kill-buffer expr-buffer))))


;; Self defvar and defun
(defmacro def-self-font (font-size)
  "Define default FONT-SIZE of current paltform, 
ignore it if you don't like it. 
eg., `Monaco-13', `Consolas-13', `White Rabbit-12'

\(fn FONT-SIZE)"
  (graphic-supported-p
    (let ((_font_ (self-symbol 'font)))
      `(defvar ,_font_ ,font-size))))

(defmacro def-self-cjk-font (font-size)
  "Define default CJK FONT-SIZE of current paltform, 
ignore it if you don't like it. 
eg., (cons \"Microsoft Yahei\" 12)

\(fn FONT-SIZE)"
  (graphic-supported-p
    (let ((_font_ (self-symbol 'cjk-font)))
      `(defvar ,_font_ ,font-size))))

(defmacro def-self-theme (theme)
  "Define default THEME of current platform, 
ignore it if you don't like it. 
eg., 'tomorrow-night-eighties, 'tomorrow-night-blue

\(fn THEME)"
  (graphic-supported-p
    (let ((_theme_ (self-symbol 'theme)))
      `(defvar ,_theme_ ,theme))))

(defmacro def-self-prelogue (&rest body)
  "Define self-prelogue, it will be run before load other 
self things.

\(fn BODY...)"
  (declare (indent 0))
  (let ((_prelogue_ (self-symbol 'prelogue)))
    `(defun ,_prelogue_ ()
       ,@body)))

(defmacro def-self-epilogue (&rest body)
  "Define self-epilogue, it will be run after load other 
self things.

\(fn BODY...)"
  (declare (indent 0))
  (let ((_epilogue_ (self-symbol 'epilogue)))
    `(defun ,_epilogue_ ()
       ,@body)))


(defmacro self-safe-call (fn)
  (let ((_fn_ (self-symbol fn)))
    `(when (fboundp ',_fn_)
       (,_fn_))))

(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list:
      :cond t ;; predicat
      :packages '(x y z) ;; package list
      :setup '(\"setup-xyz.el\") ;; predefined

\(fn SPEC...)"
  (package-supported-p 
    (let ((_spec_ (self-symbol 'package-spec)))
      `(defvar ,_spec_
         (list ,@spec)))))


;; Versionized dirs
(setq-default recentf-save-file
              (concat (make-vdir ".recentf/") "recentf"))
(setq-default savehist-file
              (concat (make-vdir ".minibuffer/") "history"))


(defun install-packages (packages &optional dry)
  "Install missing packages, returns alist of installed packages"
  (package-supported-p
    (let ((not-installed-packages
           (delete t (mapcar #'(lambda (p)
                                 (if (package-installed-p p) t p))
                             packages))))
      (when not-installed-packages
        (unless dry (package-refresh-contents))
        (message "#Installing the missing %d packages: %s"
                 (length not-installed-packages)
                 not-installed-packages)
        (mapc (lambda (i) (unless dry (package-install i)))
              not-installed-packages)
        not-installed-packages))))


(defmacro parse-package-spec (spec)
  "Returns a list of `:packages' and `:setup' from the SPEC."
  `(let ((packages nil)
         (files nil))
     (dolist (s ,spec)
       (when (and (plist-get s :cond)
                  (funcall (plist-get s :cond)))
         (setq packages (append packages (plist-get s :packages)))
         (when (plist-get s :setup)
           (setq files (append files (plist-get s :setup))))))
     (list :packages packages :setup files)))


(defmacro self-install-package ()
  "Install and setup self's packages, will be called in self's PRELOGUE" 
  `(package-supported-p

     ;; define package user dir
     (setq-default package-user-dir (make-vdir "elpa/"))
     ;; define package repositories
     (setq-default
      package-archives
      (append (list '("gnu" . "https://elpa.gnu.org/packages/")
                    '("melpa-stable" . "https://stable.melpa.org/packages/"))
              (version-supported-when
                  <= 25.1
                (list '("melpa" . "https://melpa.org/packages/")))))

     (version-supported-when
         <= 25.1
       (setq-default package-archive-priorities
                     (list '("melpa-stable" . 10)
                           '("melpa" . 5)
                           '("gnu" . 0))))
     
     ;; install basic packages
     (defvar basic-packages '(aggressive-indent
                              bing-dict
                              ido-ubiquitous
                              markdown-mode
                              paredit
                              rainbow-delimiters
                              smex
                              tagedit))
     (defvar basic-setup-files '("setup-lisp.el"
                                 "setup-navigation.el"
                                 "setup-python.el"))

     (require 'package)
     (package-initialize)

     (install-packages basic-packages)
     (compile-and-load-elisp-files basic-setup-files "config/")

     ;; install self packages
     (defvar self-packages nil)
     (defvar self-setup-files nil)
     (safe-do-when!* (self-symbol "package-spec")
       (let ((spec (parse-package-spec
                    (symbol-value (self-symbol "package-spec")))))
         (install-packages (setq self-packages (plist-get spec :packages)))
         (compile-and-load-elisp-files
          (setq self-setup-files (plist-get spec :setup)) "config/")))

     ;; set Emacs' package-selected-packages var
     (version-supported-when
         <= 25.1
       (safe-setq package-selected-packages
                  (append basic-packages self-packages)))))



;; First to load self, env parts
(compile-and-load-elisp-files '("self.el")
                              "private/")
(compile-and-load-elisp-files '("ui.el"
                                "shell.el"
                                "basic.el")
                              "config/")

;; Self do prelogue ...
(self-safe-call prelogue)


(compile-and-load-elisp-files
 ;; compile and load non-package-required elisp files
 '("editing.el"
   "debugger.el"
   "financial.el"
   "utils.el") "config/")

(comment
 (compile-and-load-elisp-files
  ;; compile and load private non-package-required elisp files
  '("financial.el"
    "utils.el") "private/"))


;; Self do epilogue ...
(self-safe-call epilogue)



;; After loaded ...



(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))



;; ^ End of init.el

