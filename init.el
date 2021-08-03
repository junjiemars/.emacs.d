;;; init.el --- init
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; Commentary:
;; common notions.
;;
;;; Code:


(defvar *loading-epoch*
  (current-time)
  "The epoch of loading init.el.")


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
               ,(concat (if (display-graphic-p) "g_" "t_")
                        emacs-version "/")
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


;; compile macro


(defmacro compile-and-load-file*
    (file &optional only-compile delete-booster dir)
  "Compile FILE.

If ONLY-COMPILE is t, does not load compiled file.
If DELETE-BOOSTER is t, remove booster file.
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
(compile-and-load-file* (emacs-home* "config/boot.el")
                        nil ;; only-compile
                        nil ;; delete-booster
                        (v-home* "config/"))


(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;; After loaded ...


(message "#Loading %s ... done (%.5fs)"
         (emacs-home* "init.el")
         (float-time (time-subtract (current-time) *loading-epoch*)))


;;; init.el ends here
