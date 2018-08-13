;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;;
;; https://github.com/junjiemars/.emacs.d
;;;;


;; basic macro

(defmacro comment (&rest body)
  "Ignores body, yields nil."
  nil)


(defvar loading-start-time
  (current-time)
  "The start time at loading init.el")


(defconst +emacs-home+
	(expand-file-name (if (boundp 'user-emacs-directory)
												user-emacs-directory
											"~/.emacs.d/"))
	"The user's emacs home directory")


(defmacro emacs-home* (&rest subdirs)
  "Return path of SUBDIRS under `+emacs-home+'."
  (declare (indent 0))
  `(concat ,+emacs-home+ ,@subdirs))


(defconst +v-dir+
	(concat (if (display-graphic-p) "g_" "t_") emacs-version)
	"Versioned dir based on [g]rahpic/[t]erminal mode and Emacs's version")


(defmacro v-home* (subdir &optional file)
  "Return versioned path of SUBDIR/`+v-dir+'/FILE under `+emacs-home+'."
  `(concat ,+emacs-home+ ,subdir ,+v-dir+ "/" ,file))


(defmacro v-home% (subdir &optional file)
  "Return versioned path of SUBDIR/`+v-dir+'/FILE under `+emacs-home+' at compile-time."
  (let ((_vfile_ (v-home* subdir file)))
    `,_vfile_))


(defmacro v-home! (subdir &optional file)
  "Make versioned SUBDIR/`+v-dir+/' directory under `+emacs-home+' and return the versioned path of SUBDIR/`+v-dir+'/file."
  (let ((_vdir_ (v-home* subdir))
        (_vfile_ (v-home* subdir file)))
    (unless (file-exists-p _vdir_)
      (make-directory _vdir_ t))
    `,_vfile_))


(defmacro file-name-base* (file)
  "Return base name of FILE with no directory, no extension."
  `(file-name-sans-extension (file-name-nondirectory ,file)))


(defmacro v-path! (file vdir &optional extension)
  "Make and return the path of FILEdirectory/VDIR/FILEbasename.EXTENSION."
  `(when (and ,vdir (file-exists-p ,file))
     (let ((v (concat (file-name-directory ,file) ,vdir "/")))
       (unless (file-exists-p v) (make-directory v t))
       (concat v (if ,extension
										 (concat (file-name-base* ,file) "." ,extension)
                   (file-name-nondirectory ,file))))))


 ;; end of basic macro


;; compile macro


(defconst +compile-lock-name+ (v-home% "config/" ".compile.lock")
	"Compile lock file be claimed when compiling process occurred.")

(defmacro compile-claim-lock ()
	"Record this Emacs pid in `+compile-lock-name+' file."
	`(unless (file-exists-p +compile-lock-name+)
		 (write-region (number-to-string (emacs-pid)) nil +compile-lock-name+)))


(defmacro compile-and-load-file* (vdir file &optional only-compile delete-booster)
  "Compile FILE and save the compiled one in VDIR then load it if ONLY-COMPILE is nil.

If DELETE-BOOSTER is non nil then delete booster source FILE after compiled."
  (let ((c (make-symbol "-compiled:0-"))
        (s (make-symbol "-source:0-")))
    `(when (and (stringp ,file) (file-exists-p ,file))
       (let ((,c (v-path! ,file ,vdir "elc")))
         (when (or (not (file-exists-p ,c))
                   (file-newer-than-file-p ,file ,c))
           (let ((,s (v-path! ,file ,vdir)))
             (copy-file ,file ,s t)
             (when (byte-compile-file ,s)
							 (compile-claim-lock)
							 (when ,delete-booster (delete-file ,s)))))
         (or ,only-compile
             (load ,c))))))


(defmacro clean-compiled-files ()
  "Clean all compiled files."
  `(dolist (d (list ,(v-home* "config/")
                    ,(v-home* "private/")
										,(v-home* "theme/")))
     (dolist (f (when (file-exists-p d)
									(directory-files d nil "\\.elc?$")))
       (message "#Clean compiled file: %s" f)
       (delete-file (concat d f)))))


 ;; end of compile macro


(defmacro progn% (&rest body)
  "Return an `progn'ed form if BODY has more than one sexp.

Else return BODY sexp."
  (if (cdr body) `(progn ,@body) (car body)))


;; version-supported macro

(defmacro version-supported* (cond version)
  "Return t if (COND VERSION `emacs-version') yields non-nil, else nil.

COND should be quoted, such as (version-supported* '<= 24)"
  `(let ((ver (cond ((stringp ,version) ,version)
		    ((numberp ,version) (number-to-string ,version))
		    (t (format "%s" ,version)))))
     (cond ((eq '< ,cond) (version< ver emacs-version))
	   ((eq '<= ,cond) (version<= ver emacs-version))
	   ((eq '> ,cond) (not (version<= ver emacs-version)))
	   ((eq '>= ,cond) (not (version< ver emacs-version)))
	   (t nil))))

(defmacro version-supported-p (cond version)
  "Return t if (COND VERSION `emacs-version') yields non-nil, else nil.

It resemble `version-supported*' but be expanded at compile time."
  (let ((x (version-supported* `,cond `,version)))
    x))

(defmacro version-supported-if (cond version then &rest else)
  "If (COND VERSION `emacs-version') yields non-nil, do THEN, else do ELSE...

Return the value of THEN or the value of the last of the ELSE’s.
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


 ;; end of version-supported macro


;; package-supported macro

(defmacro package-supported-p (&rest body)
  "Run BODY code if current `emacs-version' supports package."
  (declare (indent 0))
  `(version-supported-when <= 24.1 ,@body))

 ;; end of package-supported macro


;; lexical-supported macro

(defmacro lexical-supported-if (then &rest else)
  "If support lexical binding then do BODY, otherwise do ELSE..."
  (declare (indent 1))
  `(version-supported-if
       <= 24.1
       ,then
     (progn% ,@else)))

(defmacro lexical-supported-when (&rest body)
  "Do BODY when lexical binding, else return nil."
  (declare (indent 0))
  `(lexical-supported-if (progn% ,@body)))

(defmacro lexical-supported-unless (&rest body)
  "Do BODY unless support lexical binding."
  (declare (indent 0))
  `(lexical-supported-if nil ,@body))


 ;; end of lexical-supported macro


;; Load strap
(compile-and-load-file*
 +v-dir+
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
