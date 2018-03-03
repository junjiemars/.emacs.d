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
  "Return path of SUBDIRS under `emacs-home'."
  (declare (indent 0))
  `(concat ,emacs-home ,@subdirs))


(defvar v-dir
  (concat (if (display-graphic-p) "g_" "t_")
          emacs-version)
  "Versioned dir based on [g]rahpic/[t]erminal mode and Emacs's version")


(defmacro v-home* (subdir &optional file)
  "Return versioned path of SUBDIR/FILE under `emacs-home'."
  `(concat ,emacs-home ,subdir ,v-dir "/" ,file))


(defmacro v-home! (subdir &optional file)
  "Make the versioned path of SUBDIR/FILE under `emacs-home' and returns it."
  (let ((_vdir_ (v-home* subdir))
        (_vfile_ (v-home* subdir file)))
    (unless (file-exists-p _vdir_)
      (make-directory _vdir_ t))
    `,_vfile_))


(defmacro base-file-name (file)
  "Return base name of FILE with no directory, no extension."
  `(file-name-sans-extension (file-name-nondirectory ,file)))


(defmacro v-path! (file dir &optional extension)
  "Return versioned DIR base on existing FILE's directory and return it."
  `(when (and ,dir (file-exists-p ,file))
     (let ((v (concat (file-name-directory ,file) ,dir "/")))
       (unless (file-exists-p v) (make-directory v t))
       (concat v (if (and ,extension (file-name-extension ,file))
                     (concat (base-file-name ,file) "." ,extension)
                   (file-name-nondirectory ,file))))))





(defmacro compile-and-load-file* (vdir file &optional only-compile)
  "Compile FILE and save the compiled one in VDIR then load it.

If ONLY-COMPILE is t then do not load FILE."
  (let ((c (make-symbol "-compiled:0-"))
        (s (make-symbol "-source:0-")))
    `(when (and (stringp ,file) (file-exists-p ,file))
       (let ((,c (v-path! ,file ,vdir "elc")))
         (when (or (not (file-exists-p ,c))
                   (file-newer-than-file-p ,file ,c))
           (let ((,s (v-path! ,file ,vdir)))
             (copy-file ,file ,s t)
             (byte-compile-file ,s)))
         (or ,only-compile
             (load ,c))))))


(defmacro clean-compiled-files ()
  "Clean all compiled files."
  `(dolist (d (list ,(v-home* "config/")
                    ,(v-home* "private/")))
     (dolist (f (directory-files d nil "\\.elc$"))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))






(defmacro progn% (&rest body)
  "Return an `progn'ed form if BODY has more than one sexp.

Else return BODY sexp."
  (if (cdr body) `(progn ,@body) (car body)))


(defmacro platform-supported-if (os then &rest else)
  "If (eq system-type OS) yields non-nil, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (eq `system-type' OS) yields nil, and there are no ELSE’s, the value is nil. "
  (declare (indent 2))
  (if (eq system-type os)
      `,then
    `(progn% ,@else)))


(defmacro platform-supported-when (os &rest body)
  "Run BODY code if on specified OS platform, else return nil."
  (declare (indent 1))
  `(platform-supported-if ,os (progn% ,@body)))


(defmacro platform-supported-unless (os &rest body)
  "Run BODY code unless on specified OS platform, else return nil."
  (declare (indent 1))
  `(platform-supported-if ,os nil ,@body))


(defmacro version-supported* (cond version)
  "Return t if (COND VERSION EMACS-VERSION) yields non-nil, else nil.

COND should be quoted, such as (version-supported* '<= 24)"
  `(funcall ,cond
            (truncate (* 10 ,version))
            (+ (* 10 emacs-major-version) emacs-minor-version)))


(defmacro version-supported-p (cond version)
  "Returns t if (COND VERSION `emacs-version') yields non-nil, else nil.

It resemble `version-supported*' but it has constant runtime."
  (let ((x (version-supported* `,cond `,version)))
    x))


(defmacro version-supported-if (cond version then &rest else)
  "If (COND VERSION `emacs-version') yields non-nil, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (COND VERSION EMACS-VERSION) yields nil, and there are no ELSE’s, the value is nil. "
  (declare (indent 3))
  (if (version-supported* `,cond `,version)
      `,then
    `(progn% ,@else)))


(defmacro version-supported-when (cond version &rest body)
  "If (COND VERSION `emacs-version') yields non-nil, do BODY, else return nil.

When (COND VERSION `emacs-version') yields non-nil, eval BODY forms 
sequentially and return value of last one, or nil if there are none."
  (declare (indent 2))
  `(version-supported-if ,cond ,version (progn% ,@body)))


(defmacro graphic-supported-if (then &rest else)
  "If in graphic mode, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If in terminal mode, and there are no ELSE’s, the value is nil. "
  (declare (indent 1))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))


(defmacro graphic-supported-p (&rest body)
  "Run BODY code if in graphic mode, else returns nil."
  (declare (indent 0))
  `(graphic-supported-if (progn% ,@body)))


(defmacro terminal-supported-p (&rest body)
  "Run BODY code if in terminal mode, else returns nil."
  (declare (indent 0))
  `(graphic-supported-if nil ,@body))





(defmacro safe-call (fn &rest args)
  "Call FN with ARGS if FN has already been bound."
  (declare (indent 1))
  (if (fboundp fn)
      `(,fn ,@args)))


(defmacro safe-fn-if (fn then &rest else)
  "If FN is bounded yields non-nil, do THEN, else do ELSE...

Returns the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If FN is not bounded yields nil, and there are no ELSE’s, the value is nil."
  (declare (indent 2))
  (if (fboundp fn)
      `,then
    `(progn% ,@else)))


(defmacro safe-fn-when (fn &rest body)
  "Do BODY when FN is bound."
  (declare (indent 1))
  `(safe-fn-if ,fn (progn% ,@body)))


(defmacro safe-fn-unless (fn &rest body)
  "Do BODY unless FN is bound."
  (declare (indent 1))
  `(safe-fn-if ,fn nil ,@body))


(defmacro safe-fn-when* (fn &rest body)
  "Do BODY when FN is local bound."
  (declare (indent 1))
  `(when (fboundp ,fn) ,@body))


(defmacro safe-var-when! (x &rest body)
  "Do BODY when X is bound."
  (declare (indent 1))
  (when (boundp x)
    `(progn% ,@body)))


(defmacro safe-setq (x val)
  "Set X when variable X is bound."
  (when (boundp x)
    `(setq ,x ,val)))


(defmacro safe-setq* (x val)
  "Set X when variable X is bound."
  `(when (boundp (quote ,x))
     (setq ,x ,val)))




(defmacro package-supported-p (&rest body)
  "Run BODY code if current `emacs-version' supports package."
  (declare (indent 0))
  `(version-supported-when <= 24.1 ,@body))


(defmacro self-symbol (name)
  `(intern (format "self-%s-%s" system-type ,name)))


(defmacro def-self-path-ref (&rest path)
  "Define the PATH references for all specs in `self-path.el'."
  (declare (indent 0))
  (let ((_path_ (self-symbol 'path)))
    `(defvar ,_path_ (list ,@path))))


(defmacro def-self-env-spec (&rest spec)
  "Define default Emacs env SPEC of current platform on current `emacs-version'."
  (declare (indent 0))
  (let ((_spec_ (self-symbol 'env-spec)))
    `(defvar ,_spec_ (list ,@spec))))


(defmacro def-self-package-spec (&rest spec)
  "Define self package SPEC list."
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
         (when *val* ,@body)))))




;; Load strap
(compile-and-load-file*
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
