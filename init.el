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
  "Returns the path of SUBDIRS under `emacs-home'."
  (declare (indent 0))
  `(concat ,emacs-home ,@subdirs))


(defvar v-dir
  (concat (if (display-graphic-p) "g_" "t_")
          emacs-version)
  "Versioned dir based on [g]rahpic/[t]erminal mode and Emacs's version")


(defmacro v-home* (subdir &optional file)
  "Returns the versioned path of SUBDIR/FILE under `emacs-home'."
  `(concat ,emacs-home ,subdir ,v-dir "/" ,file))


(defmacro v-home! (subdir &optional file)
  "Make the versioned path of SUBDIR/FILE under `emacs-home' and returns it."
  (let ((_vdir_ (v-home* subdir))
        (_vfile_ (v-home* subdir file)))
    (when (not (file-exists-p _vdir_))
      (make-directory _vdir_ t))
    `,_vfile_))


(defmacro base-file-name (file)
  "Returns the base name of the FILE with no directory, no extension."
  `(file-name-sans-extension (file-name-nondirectory ,file)))


(defmacro v-path! (file dir &optional extension)
  "Make the versioned DIR base on the existing FILE's directory and returns it."
  `(when (and ,dir (file-exists-p ,file))
     (let ((v (concat (file-name-directory ,file) ,dir "/")))
       (when (not (file-exists-p v))
         (make-directory v t))
       (concat v (if (and ,extension (file-name-extension ,file))
                     (concat (base-file-name ,file) "." ,extension)
                   (file-name-nondirectory ,file))))))




(defmacro compile-and-load-elisp-file* (vdir file)
  "Compile and load the elisp FILE, save compiled files in VDIR."
  `(if (and (stringp ,file) (file-exists-p ,file))
       (let ((c (v-path! ,file ,vdir "elc")))
         (when (or (not (file-exists-p c))
                   (file-newer-than-file-p ,file c))
           (let ((s (v-path! ,file ,vdir)))
             (copy-file ,file s t)
             (byte-compile-file s)))
         (if (file-exists-p c)
             (load c)
           (message "#Missing %s" c)))
     (message "#Skip compile and load %s.done" ,file)))


(defmacro clean-compiled-files ()
  "Clean all compiled files."
  `(dolist (d (list ,(v-home* "config/")
                    ,(v-home* "private/")))
     (dolist (f (directory-files d nil "\\.elc$"))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))






(defmacro progn% (&rest exps)
  "Returns an `progn'ed expression where EXPS has more than one expressions.

Else returns the only one EXPR."
  (if (cdr exps) `(progn ,@exps) (car exps)))


(defmacro platform-supported-if (os then &rest else)
  "If (eq system-type OS) yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (eq system-type OS) yields nil, and there are no ELSE’s, the value is nil.

\(fn OS THEN ELSE...)"
  (declare (indent 2))
  (if (eq system-type os)
      `,then
    `(progn% ,@else)))


(defmacro platform-supported-when (os &rest body)
  "Run BODY code if on specified OS platform.

\(fn OS BODY...)"
  (declare (indent 1))
  `(platform-supported-if ,os (progn% ,@body)))


(defmacro platform-supported-unless (os &rest body)
  "Run BODY code unless on specified OS platform.

\(fn OS BODY...)"
  (declare (indent 1))
  `(platform-supported-if ,os nil ,@body))


(defmacro version-supported* (cond version)
  "Return true if (COND VERSION EMACS-VERSION) yields non-nil, else nil.

\(fn COND VERSION)"
  `(funcall ,cond
            (truncate (* 10 ,version))
            (+ (* 10 emacs-major-version) emacs-minor-version)))


(defmacro version-supported-p (cond version)
  "Return true if (COND VERSION EMACS-VERSION) yields non-nil, else nil.
It resemble `version-supported*' but it has constant runtime.

\(fn COND VERSION)"
  (let ((x (version-supported* `,cond `,version)))
    x))


(defmacro version-supported-if (cond version then &rest else)
  "If (COND VERSION EMACS-VERSION) yields non-nil, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (COND VERSION EMACS-VERSION) yields nil, and there are no ELSE’s, the value is nil.

\(fn COND VERSION THEN ELSE...)"
  (declare (indent 3))
  (if (version-supported* `,cond `,version)
      `,then
    `(progn% ,@else)))


(defmacro version-supported-when (cond version &rest body)
  "If (COND VERSION EMACS-VERSION) yields non-nil, do BODY, else return nil.
When (COND VERSION EMACS-VERSION) yields non-nil, eval BODY forms 
sequentially and return value of last one, or nil if there are none.

\(fn COND VERSION BODY...)"
  (declare (indent 2))
  `(version-supported-if ,cond ,version (progn% ,@body)))


(defmacro graphic-supported-if (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...
Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If in terminal mode, and there are no ELSE’s, the value is nil.

\(fn THEN ELSE...)"
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))


(defmacro graphic-supported-p (&rest body)
  "Run BODY code if in graphic mode.

\(fn BODY...)"
  (declare (indent 0))
  `(graphic-supported-if (progn% ,@body)))


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



(defmacro string-trim> (s &optional rr)
  "Remove whitespaces or the matching of RR at the end of S.

\(fn STRING &optional RIGHT-REGEXP)"
  `(let ((r (if ,rr (concat ,rr "\\'")
              "[ \t\n\r]+\\'" )))
     (if (string-match r ,s)
         (replace-match "" t t ,s)
       ,s)))


(defmacro bin-path (b)
  "Returns the path of BIN-NAME in env.

\(fn BIN-NAME)"
  (platform-supported-if windows-nt
      (let ((_b_ (concat "where " `,b)))
        `(string-trim> (shell-command-to-string ,_b_)))
    (let ((_b_ (concat "type -P " `,b)))
      `(string-trim> (shell-command-to-string ,_b_)))))




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
    `(progn% ,@else)))


(defmacro safe-fn-when (fn &rest body)
  "Do BODY when FN is bound.

\(fn FN BODY...)"
  (declare (indent 1))
  `(safe-fn-if ,fn (progn% ,@body)))


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
    `(progn% ,@body)))


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




(defmacro package-supported-p (&rest body)
  "Run BODY code if supports package.

\(fn BODY...)"
  (declare (indent 0))
  `(version-supported-when <= 24.1 ,@body))


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defmacro def-self-path (path)
  "Define the PATH of where your `self.el'.
 
\(fn PATH)"
  (declare (indent 0))
  (let ((_path_ (self-symbol 'path)))
    `(when ,path
       (defvar ,_path_ ,path))))


(defmacro def-self-env-spec (&rest spec)
  "Define default Emacs env SPEC of current platform on current Emacs version, 
ignore it if you don't like it. 
 
\(fn SPEC)"
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


(defmacro self-safe-call (fn)
  (let ((_fn_ (self-symbol fn)))
    `(when (fboundp ',_fn_)
       (,_fn_))))


(defmacro self-safe-call* (var &rest body)
  (let ((_var_ (self-symbol var)))
    (when (boundp _var_)
      `(let ((*val* (symbol-value ',_var_)))
         ,@body))))




;; Load strap
(compile-and-load-elisp-file*
 v-dir
 (emacs-home* "config/strap.el"))


(package-supported-p
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;; After loaded ...

(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))



;; ^ End of init.el
