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

(defvar *gensym-counter* 0
  "The counter of `gensym*'.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol.
The name is made by appending a number to PREFIX, default \"g\"."
  (let ((pfix (if (stringp prefix) prefix "g"))
        (num (if (integerp prefix) prefix
               (prog1 *gensym-counter*
                 (setq *gensym-counter* (1+ *gensym-counter*))))))
    (make-symbol (format "%s%d" pfix num))))


(defvar loading-start-time
  (current-time)
  "The start time at loading init.el")


 ;; end of basic macro

;; file macro

(defmacro emacs-home* (&rest subdirs)
  "Return path of SUBDIRS under `emacs-home'."
  (declare (indent 0))
  `(concat ,(expand-file-name (if (boundp 'user-emacs-directory)
                        user-emacs-directory
                      "~/.emacs.d/")) ,@subdirs))


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
  (let ((d (gensym*)))
    `(let ((,d (if (directory-name-p ,file)
                   ,file
                 (file-name-directory ,file))))
       (unless (file-exists-p ,d)
         (make-directory ,d t))
       ,file)))


 ;; end basic file macro


;; versioned file macro


(defmacro v-path* (file &optional extension)
  "Return versioned FILE with new EXTENSION."
  `(concat (if (directory-name-p ,file)
               ,file
             (file-name-directory ,file))
           ,(concat (if (display-graphic-p) "g_" "t_") emacs-version) "/"
           (if ,extension
               (file-name-new-extension* (file-name-nondirectory ,file)
                                         ,extension)
             (file-name-nondirectory ,file))))


(defmacro v-home* (file)
  "Return versioned path of `emacs-home'/`v-path*'/FILE."
  `(v-path* (emacs-home* ,file)))


(defmacro v-home% (file)
  "Return versioned path of `emacs-home'/`v-path*'/FILE at compile-time."
  (let ((_vfile%_ (v-home* file)))
    `,_vfile%_))


(defmacro v-home! (file)
  "Make versioned `emacs-home'/`v-path*'/FILE at compile-time."
  (let ((_vfile!_ (path! (v-home* file))))
    `,_vfile!_))


 ;; end of versioned file macro


;; compile macro


(defmacro compile-and-load-file* (file &optional only-compile delete-booster dir)
  "Compile FILE.

If ONLY-COMPILE is t, does not load COMPILED file after compile FILE.
If DELETE-BOOSTER is t, remove booster file after compile FILE.
DIR where the compiled file located."
  (let ((c (gensym*))
        (s (gensym*)))
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


;; compile-time macro

(defmacro progn% (&rest body)
  "Return an `progn'ed form if BODY has more than one sexp.

Else return BODY sexp."
  (if (cdr body) `(progn ,@body) (car body)))


(defmacro if% (cond then &rest else)
  "If COND yields non-nil, do THEN, else do ELSE..."
  (declare (indent 2))
  (if (funcall `(lambda () ,cond))
      `,then
    `(progn% ,@else)))


(defmacro when% (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (declare (indent 1))
  `(if% ,cond (progn% ,@body)))


(defmacro unless% (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (declare (indent 1))
  `(if% ,cond nil ,@body))


 ;; end of compile-time macro


;; version-supported macro

(defmacro version-supported-if (cmp version then &rest else)
  "If \(CMP VERSION `emacs-version'\) yields non-nil, do THEN, else do ELSE...

Return the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (COND VERSION EMACS-VERSION) yields nil, and there are no ELSE’s, the value is nil. "
  (declare (indent 3))
  (let ((ver (cond ((numberp version) (number-to-string version))
                   ((stringp version) version)
                   (t (format "%s" (funcall `(lambda () ,version)))))))
    `(if% (cond ((eq '< ',cmp) (version< ,ver emacs-version))
                ((eq '<= ',cmp) (version<= ,ver emacs-version))
                ((eq '> ',cmp) (not (version<= ,ver emacs-version)))
                ((eq '>= ',cmp) (not (version< ,ver emacs-version)))
                (t nil))
         ,then
       (progn% ,@else))))

(defmacro version-supported-when (cmp version &rest body)
  " When \(CMP VERSION `emacs-version'\) yields non-nil, eval BODY forms 
sequentially and return value of last one, or nil if there are none."
  (declare (indent 2))
  `(version-supported-if ,cmp ,version (progn% ,@body)))


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


(message "#Loading init.el ... done (%.5fs)"
         (float-time (time-subtract (current-time) loading-start-time)))



;; ^ End of init.el
