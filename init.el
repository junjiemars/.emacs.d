;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;;
;; https://github.com/junjiemars/.emacs.d
;;;;
;; init.el
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


 ;; end of basic macrob

;; file macro

(defmacro emacs-home* (&rest subdirs)
  "Return path of SUBDIRS under `+emacs-home+'."
  (declare (indent 0))
  `(concat ,+emacs-home+ ,@subdirs))


(defmacro file-name-base* (file)
  "Return base name of FILE with no directory, no extension."
  `(file-name-sans-extension (file-name-nondirectory ,file)))


(defmacro file-name-new-extension* (file extension)
	"Return FILE name with new EXTENSION."
	`(concat (file-name-directory ,file)
					 (file-name-base* ,file)
					 ,extension))


(unless (fboundp 'directory-name-p)
	(defmacro directory-name-p (name)
		"Returns t if NAME ends with a directory separator character."
		`(let ((len (length ,name)))
			 (and (> len 0) (= ?/ (aref ,name (1- len)))))))


(defmacro path! (file)
  "Make and return the path of the FILE.

The FILE should be posix path, see `path-separator'."
	(let ((d (make-symbol "-dir:0-")))
		`(let ((,d (if (directory-name-p ,file)
									 ,file
								 (file-name-directory ,file))))
			 (unless (file-exists-p ,d)
				 (make-directory ,d t))
			 ,file)))


 ;; end basic file macro


;; versioned file macro


(defconst +v-dir+
	(concat (if (display-graphic-p) "g_" "t_") emacs-version)
	"Versioned dir based on [g]rahpic/[t]erminal mode and Emacs's version")


(defmacro v-path* (file &optional extension)
	"Return versioned FILE with new EXTENSION."
	`(concat (if (directory-name-p ,file)
							 ,file
						 (file-name-directory ,file))
					 +v-dir+ "/"
					 (if ,extension
							 (file-name-new-extension* (file-name-nondirectory ,file)
																				 ,extension)
						 (file-name-nondirectory ,file))))


(defmacro v-home* (file)
  "Return versioned path of `+emacs-home+'/`+v-dir+'/FILE."
	`(v-path* (emacs-home* ,file)))


(defmacro v-home% (file)
  "Return versioned path of `+emacs-home+'/`+v-dir+'/FILE at compile-time."
  (let ((_vfile%_ (v-home* file)))
    `,_vfile%_))


(defmacro v-home! (file)
  "Make versioned `+emacs-home+'/`+v-dir+'/FILE at compile-time."
  (let ((_vfile!_ (path! (v-home* file))))
    `,_vfile!_))


 ;; end of versioned file macro


;; compile macro


(defmacro compile-and-load-file* (file &optional only-compile delete-booster dir)
  "Compile FILE.

If ONLY-COMPILE is t, does not load COMPILED file after compile FILE.
If DELETE-BOOSTER is t, remove booster file after compile FILE.
DIR where the compiled file located."
  (let ((c (make-symbol "-compile:0-"))
				(s (make-symbol "-source:0-")))
    `(when (and (stringp ,file) (file-exists-p ,file))
			 (let ((,c (if ,dir
										 (file-name-new-extension*
											(concat ,dir (file-name-nondirectory ,file)) ".elc")
									 (file-name-new-extension* ,file ".elc"))))
				 (when (or (not (file-exists-p ,c))
									 (file-newer-than-file-p ,file ,c))
					 (let ((,s (if ,dir
												 (concat ,dir (file-name-nondirectory ,file))
											 ,file)))
						 (unless (string= ,file ,s) (copy-file ,file (path! ,s) t))
						 (when (byte-compile-file ,s)
							 (when ,delete-booster (delete-file ,s)))))
				 (when (file-exists-p ,c)
					 (cond (,only-compile t)
								 (t (load ,c))))))))


(defmacro clean-compiled-files ()
  "Clean all compiled files."
  `(dolist (d (list ,(v-home* "config/")
                    ,(v-home* "private/")
										,(v-home* "theme/")
										,(v-home* ".exec/")))
     (dolist (f (when (file-exists-p d)
									(directory-files d nil "\\.elc?\\'")))
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


;; Load strap
(compile-and-load-file* (emacs-home* "config/strap.el")
												nil ;; only-compile
												nil ;; delete-booster
												(v-home* "config/"))


(package-supported-p
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;; After loaded ...

(let ((elapsed
       (float-time
        (time-subtract (current-time) loading-start-time))))
  (message "#Loading init.el ... done (%.3fs)" elapsed))



;; ^ End of init.el
