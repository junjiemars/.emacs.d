;;; init.el --- init
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; Commentary:
;; common notions.
;;
;;; Code:


(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)


(defvar *gensym-counter* 0
  "The counter of `gensym*'.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"g\"."
  (let ((num (prog1 *gensym-counter*
               (setq *gensym-counter* (1+ *gensym-counter*)))))
    (make-symbol (format "%s%d" (or prefix "g") num))))


;; file macro

(defmacro emacs-home* (&rest subdirs)
  "Return path of SUBDIRS under `emacs-home'."
  (declare (indent 0))
  `(concat ,(expand-file-name (if (boundp 'user-emacs-directory)
                                  user-emacs-directory
                                "~/.emacs.d/"))
           ,@subdirs))


(defmacro file-name-base* (file)
  "Return base name of FILE without directory and extension."
  `(file-name-sans-extension (file-name-nondirectory ,file)))


(defmacro file-name-new-extension* (file extension)
  "Return FILE name with new EXTENSION."
  (let ((f (gensym*)))
    `(let ((,f ,file))
       (concat (file-name-directory ,f)
               (file-name-base* ,f)
               ,extension))))


(unless (fboundp 'directory-name-p)
  (defmacro directory-name-p (name)
    "Return t if NAME ends with a directory separator character."
    (let ((n (gensym*))
          (w (gensym*)))
      `(let* ((,n ,name)
              (w (length ,n)))
         (and (> w 0) (= ?/ (aref ,n (1- w))))))))


(defmacro path! (file)
  "Make and return the path of the FILE.

The FILE should be posix path, see `path-separator'."
  (let ((f (gensym*))
        (d (gensym*)))
    `(let* ((,f ,file)
            (,d (if (directory-name-p ,f)
                    ,f
                  (file-name-directory ,f))))
       (unless (file-exists-p ,d)
         (make-directory ,d t))
       ,f)))


 ;; end basic file macro


;; versioned file macro


(defmacro v-name ()
  "Return the versioned name."
  `(concat (if (display-graphic-p) "g_" "t_") emacs-version))


(defmacro v-path* (file &optional extension)
  "Return versioned FILE with new EXTENSION."
  (let ((f (gensym*))
        (n (gensym*))
        (x (gensym*)))
    `(let* ((,f ,file)
            (,n (file-name-nondirectory ,f))
            (,x ,extension))
       (concat (if (directory-name-p ,f)
                   ,f
                 (file-name-directory ,f))
               ,(concat (v-name) "/")
               (if ,x
                   (file-name-new-extension* ,n ,x)
                 ,n)))))


(defmacro v-home* (file)
  "Return versioned path of `emacs-home'/`v-path*'/FILE."
  (let ((h (gensym*)))
    `(let ((,h (emacs-home* ,file)))
       (v-path* ,h))))


(defmacro v-home% (file)
  "Return versioned path of `emacs-home'/`v-path*'/FILE at compile-time."
  (let ((_vfile%_ (v-home* file)))
    `,_vfile%_))


(defmacro v-home! (file)
  "Make versioned `emacs-home'/`v-path*'/FILE at compile-time."
  (let ((f (gensym*)))
    `(let ((,f (path! (v-home* ,file))))
       ,f)))


 ;; end of versioned file macro


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
  "When COND yields non-nil, do BODY."
  (declare (indent 1))
  `(if% ,cond (progn% ,@body)))


(defmacro unless% (cond &rest body)
  "Unless COND yields nil, do BODY."
  (declare (indent 1))
  `(if% ,cond nil ,@body))


 ;; end of compile-time macro


;; compile macro

(defmacro if-native-comp% (then &rest else)
  "If native compilation support is built-in do THEN, else do ELSE..."
  (if (fboundp 'native-comp-available-p)
      (and (native-comp-available-p)
           (featurep 'native-compile)
           `,then)
    (progn% `,@else)))

(defmacro when-native-comp% (&rest body)
  "When native compilation support is built-in, do BODY."
  (declare (indent 0))
  `(if-native-comp% (progn% ,@body)))

(defmacro unless-native-comp% (&rest body)
  "When native compilation support is built-in, do BODY."
  (declare (indent 0))
  `(if-native-comp% nil ,@body))


(defmacro compile-and-load-file*
    (file &optional only-compile delete-booster dir)
  "Compile FILE.

If ONLY-COMPILE is t, does not load compiled file.
If DELETE-BOOSTER is t, remove booster file.
DIR where the compiled file located."
  (let ((f (gensym*))
        (d (gensym*))
        (n (gensym*))
        (c (gensym*))
        (s (gensym*)))
    `(let ((,f ,file))
       (when (and (stringp ,f) (file-exists-p ,f))
         (let* ((,n (file-name-nondirectory ,f))
                (,d ,dir)
                (,s (if ,d
                        (concat ,d ,n)
                      ,f))
                (,c (file-name-new-extension* ,s ".elc")))
           (when (or (not (file-exists-p ,c))
                     (file-newer-than-file-p ,f ,c))
             (unless (string= ,f ,s)
               (copy-file ,f (path! ,s) t))
             (when (byte-compile-file ,s)
               ;; (when-native-comp% (native-compile-async ,s nil t))
               (when ,delete-booster (delete-file ,s))))
	         (when (file-exists-p ,c)
             (cond (,only-compile t)
                   (t (load ,c)))))))))


(defun clean-compiled-files ()
  "Clean all compiled files."
  (interactive)
  (let ((dirs (list (v-home* "config/")
                    (v-home* "private/")
                    (v-home* "theme/")
                    (v-home* ".exec/"))))
    (while dirs
      (let* ((d (car dirs))
             (fs (when (file-exists-p d)
                   (directory-files d nil "\\.elc?\\'"))))
        (message "#Clean compiled files: %s..." d)
        (while fs
          (let ((f (car fs)))
            (delete-file (concat d f)))
          (setq fs (cdr fs))))
      (setq dirs (cdr dirs)))))


 ;; end of compile macro




;; *-version% macro

(defmacro if-version% (cmp version then &rest else)
  "If VERSION CMP with variable `emacs-version' is t, do THEN, else do ELSE...

Return the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (COND VERSION EMACS-VERSION) yield nil, and there are no ELSE’s, the value is nil."
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

(defmacro when-version% (cmp version &rest body)
  "When VERSION CMP with variable `emacs-version' yield non-nil, do BODY."
  (declare (indent 2))
  `(if-version% ,cmp ,version (progn% ,@body)))


 ;; end of *-version% macro


;; *-package% macro

(defmacro when-package% (&rest body)
  "If Emacs support package, do BODY."
  (declare (indent 0))
  `(when-version% <= 24.1 ,@body))

 ;; end of *-package% macro


;; Boot

(when-native-comp%

  ;; slient native-comp warning
  (setq native-comp-async-report-warnings-errors 'silent)

  ;; first native-comp load
  (setcar native-comp-eln-load-path (v-home! ".eln/")))


(compile-and-load-file* (emacs-home* "config/boot.el")
                        nil ;; compile-option
                        nil ;; delete-booster
                        (v-home* "config/"))


(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;; After loaded ...


;;; init.el ends here
