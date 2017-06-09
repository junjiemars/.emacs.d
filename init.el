;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;;;;




(defmacro comment (&rest body)
  "Ignores body, yields nil."
  nil)


(defvar loading-start-time
  (current-time)
  "The start time at loading init.el")


(defvar emacs-home
  (if (boundp 'user-emacs-directory)
      user-emacs-directory
    "~/.emacs.d/")
  "The user's emacs home directory")


(defmacro emacs-home* (&rest subdirs)
  "Return the path under `emacs-home'."
  (declare (indent 0))
  `(concat ,emacs-home ,@subdirs))


(defvar vdir
  (concat (if (display-graphic-p) "g_" "t_")
          emacs-version)
  "Versionized dir based on grahpic/terminal mode and Emacs's version")


(defmacro vdir* (subdir &optional file)
  "Return the versionized SUBDIR under emacs-home."
  `(concat ,emacs-home ,subdir ,vdir "/" ,file))


(defmacro vdir! (subdir &optional file)
  "Make the versionized SUBDIR under emacs-home and return it. "
  (let ((_vdir_ (vdir* subdir))
        (_vfile_ (vdir* subdir file)))
    (when (not (file-exists-p _vdir_))
      (make-directory _vdir_ t))
    `,_vfile_))


(defmacro compile-and-load-elisp-files (files subdir)
  "Compile and load the elisp FILES under the SUBDIR."
  `(let ((d ,(emacs-home* subdir))
         (v (vdir! ,subdir)))
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


(defmacro clean-compiled-files ()
  "Clean all compiled files, need restart Emacs."
  `(dolist (d (list ,(vdir* "config/")
                    ,(vdir* "private/")))
     (dolist (f (directory-files d nil "\\.elc$"))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))


(defmacro clean-saved-user-files ()
  "Clean saved desktop, need restart Emacs."
  `(let ((dirs (list ,(vdir*  ".auto-save/")
                     ,(vdir*  ".desktop/")
                     ,(vdir*  ".bookmarks/")
                     ,(vdir*  ".ido/")
                     ,(vdir*  ".minibuffer/")
                     ,(vdir*  ".recentf/")
                     ,(vdir*  ".tags/")
                     ,(vdir*  ".places/")
                     ,(vdir*  ".smex/")
                     ,(vdir*  ".url/"))))
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
  `(funcall ,cond
            (truncate (* 10 ,version))
            (+ (* 10 emacs-major-version) emacs-minor-version)))


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
When (COND VERSION EMACS-VERSION) yields non-nil, eval BODY forms 
sequentially and return value of last one, or nil if there are none.

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
  "Returns true if BIN-NAME exists in env.

\(fn BIN-NAME)"
  (platform-supported-if windows-nt
      (let ((_b_ (concat "where " `,b " >nul 2>&1")))
        `(zerop (shell-command ,_b_)))
    (let ((_b_ (concat "hash " `,b " &>/dev/null")))
      `(zerop (shell-command ,_b_)))))


(defmacro trim-right-newline (s)
  `(replace-regexp-in-string "\n$" "" ,s))


(defmacro bin-path (b)
  "Returns the path of BIN-NAME in env.

\(fn BIN-NAME)"
  (platform-supported-if windows-nt
      (let ((_b_ (concat "where " `,b)))
        `(trim-right-newline (shell-command-to-string ,_b_)))
    (let ((_b_ (concat "type -P " `,b)))
      `(trim-right-newline (shell-command-to-string ,_b_)))))


(defmacro safe-call (fn &rest args)
  "Call FN with ARGS if FN has already been bound.

\(fn FN-NAME ARGS...)"
  (declare (indent 1))
  (if (fboundp fn)
      `(,fn ,@args)))


(defmacro safe-fn-if (fn then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If FN is not bounded yields nil, and there are no ELSE’s, the value is nil.

\(fn FN THEN ELSE...)"
  (declare (indent 2))
  (if (fboundp fn)
      `,then
    `(progn ,@else)))


(defmacro safe-fn-when (fn &rest body)
  "Do BODY when FN is bound.

\(fn FN BODY...)"
  (declare (indent 1))
  `(safe-fn-if ,fn (progn ,@body)))


(defmacro safe-fn-unless (fn &rest body)
  "Do BODY unless FN is bound.

\(fn FN BODY...\)"
  (declare (indent 1))
  `(safe-fn-if ,fn nil ,@body))


(defmacro safe-fn-when* (fn &rest body)
  "Do BODY when FN is local bound.

\(fn FN-LOCAL BODY...)"
  (declare (indent 1))
  `(when (fboundp ,fn) ,@body))


(defmacro safe-var-when! (x &rest body)
  "Do BODY when X is bound.

\(fn X BODY...)"
  (declare (indent 1))
  (when (boundp x)
    `(progn ,@body)))


(defmacro safe-setq (x val)
  "Set X when variable X is bound.

\(fn X VAL)"
  (when (boundp x)
    `(setq ,x ,val)))


(defmacro safe-setq* (x val)
  "Set X when variable X is bound.

\(fn X VAL)"
  `(when (boundp (quote ,x))
     (setq ,x ,val)))


(defmacro safe-setq% (x val &optional comment)
  "Set X when variable X is bound.

\(fn X VAL)"
  `(safe-call customize-set-variable ,x ,val ,comment))


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defmacro def-self-env-spec (&rest spec)
  "Define default Emacs env SPEC of current platform on current Emacs version, 
ignore it if you don't like it. 
 
\(FN SPEC)"
  (declare (indent 0))
  (let ((_spec_ (self-symbol 'env-spec)))
    `(defvar ,_spec_ (list ,@spec))))


(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list:
     :cond t ;; predicat
     :packages '(x y z) ;; package list
     :setup '(\"setup-xyz.el\") ;; predefined

\(fn SPEC...)"
  (declare (indent 0))
  (package-supported-p 
    (let ((_spec_ (self-symbol 'package-spec)))
      `(defvar ,_spec_ (list ,@spec)))))


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


(defmacro self-safe-call* (var &rest body)
  (let ((_var_ (self-symbol var)))
    `(when (boundp ',_var_)
       (let ((_val_ (symbol-value ',_var_)))
         ,@body))))



;; Versionized dirs
(setq-default recentf-save-file (vdir! ".recentf/" "recentf"))
(setq-default savehist-file (vdir! ".minibuffer/" "history"))



;; Load self env
(compile-and-load-elisp-files '("self.el") "private/")


;; Load ui, shell, basic env:
(compile-and-load-elisp-files '("boot.el"
                                "shell.el"
                                "basic.el")
                              "config/")

;; Self do prelogue ...
(self-safe-call prelogue)


(package-supported-p
  ;; Basic and self package setup
  ;;(package-initialize)
  (compile-and-load-elisp-files '("module.el")  "config/"))


(compile-and-load-elisp-files
 ;; Compile and load non-package-required elisp files
 '("editing.el"
   "debugger.el"
   "financial.el"
   "tags.el"
   "utils.el") "config/")


;; Self do epilogue ...
(self-safe-call epilogue)


;; After loaded ...

(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))



;; ^ End of init.el



