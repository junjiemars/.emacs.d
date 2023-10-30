;;; init.el --- init -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; Commentary: common notions.
;;;;


(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)


(defvar *gensym-counter* 0 "The counter of \\=`gensym*\\='.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"g\"."
  (make-symbol (format "%s%d" (or prefix "g")
                       (prog1 *gensym-counter*
                         (setq *gensym-counter*
                               (1+ *gensym-counter*))))))


;;; file macro

(defmacro emacs-home* (&rest subdirs)
  "Return path of SUBDIRS under \\=`emacs-home\\='."
  (declare (indent 0))
  `(concat ,(expand-file-name (if (boundp 'user-emacs-directory)
                                  user-emacs-directory
                                "~/.emacs.d/"))
           ,@subdirs))


(defmacro file-name-new-extension (file extension)
  "Return FILE name with new EXTENSION."
  (let ((f (gensym*))
        (x (gensym*)))
    `(let* ((,f ,file)
            (,x ,extension)
            (l (length ,f)))
       (when (> l 0)
         (let* ((i (1- l))
                (d (catch 'block
                     (while (> i 0)
                       (cond ((= ?/ (aref ,f i)) (throw 'block l))
                             ((= ?. (aref ,f i)) (throw 'block i))
                             (t (setq i (1- i)))))
                     l)))
           (concat (substring-no-properties ,f 0 d)
                   (or ,x (substring-no-properties ,f d))))))))


(unless (fboundp 'directory-name-p)
  (defmacro directory-name-p (name)
    "Return t if NAME ends with a directory separator character."
    (let ((n (gensym*))
          (w (gensym*)))
      `(let* ((,n ,name)
              (w (length ,n)))
         (and (> w 0) (= ?/ (aref ,n (1- w))))))))


(defmacro path! (file)
  "Make and return the path of the FILE.\n
The FILE should be posix path, see \\=`path-separator\\='."
  (let ((f (gensym*)) (d (gensym*))
        (i (gensym*)) (ds (gensym*)))
    `(let* ((,f ,file)
            (,d (file-name-directory ,f)))
       (unless (file-exists-p ,d)
				 (let ((,i (1- (length ,d)))
               (,ds nil))
           (catch 'break
					   (while (> ,i 0)
						   (when (= ?/ (aref ,d ,i))
							   (let ((s (substring-no-properties ,d 0 (1+ ,i))))
								   (if (file-exists-p s)
                       (throw 'break t)
                     (setq ,ds (cons s ,ds)))))
						   (setq ,i (1- ,i))))
           (while (car ,ds)
             (make-directory-internal (car ,ds))
             (setq ,ds (cdr ,ds)))))
       ,f)))


;; end basic file macro


;;; *-version% macro

(defconst +emacs-version+ (string-to-number emacs-version)
  "The version of Emacs in \\=`float\\='.")

(defmacro if-version% (cmp version then &rest else)
  "If VERSION CMP with variable \\=`emacs-version\\=' is t, do THEN, else do ELSE...\n
Return the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more expressions.
If (CMP VERSION \\=`emacs-version\\=') yield nil, and there are no ELSE’s,
the value is nil."
  (declare (indent 3))
  `(if% (,cmp ,version +emacs-version+)
       ,then
     (progn% ,@else)))

(defmacro when-version% (cmp version &rest body)
  "When VERSION CMP with variable \\=`emacs-version\\=' yield non-nil, do BODY."
  (declare (indent 2))
  `(if-version% ,cmp ,version (progn% ,@body)))

(defmacro v-name ()
  "Return the versioned name."
  `(concat (if (display-graphic-p) "g_" "t_")
           (number-to-string +emacs-version+)))

;; end of *-version% macro


;;; versioned file macro

(defmacro v-path* (file &optional extension)
  "Return versioned FILE with new EXTENSION."
  (let ((f (gensym*)))
    `(let ((,f (file-name-new-extension ,file ,extension)))
       (concat (file-name-directory ,f)
               ,(v-name) "/"
               (file-name-nondirectory ,f)))))


(defmacro v-home* (file)
  "Return versioned path of (emacs-home*)/(v-path*)/FILE."
  (let ((h (gensym*)))
    `(let ((,h (emacs-home* ,file)))
       (v-path* ,h))))


(defmacro v-home% (file)
  "Return versioned (emacs-home*)/(v-path*)/FILE at compile-time."
  (v-home* file))

(defmacro v-home! (file)
  "Make versioned (emacs-home*)/(v-path*)/FILE at compile-time."
  (path! (v-home* file)))


;; end of versioned file macro


;;; compile-time macro

(defmacro progn% (&rest body)
  "Return an \\=`progn\\='ed form if BODY has more than one sexp.

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


;;; compile macro

(defmacro if-native-comp% (then &rest else)
  "If native compilation support is built-in do THEN, else do ELSE..."
  (declare (indent 1))
  (if (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
      `,then
    `(progn% ,@else)))

(defmacro when-native-comp% (&rest body)
  "When native compilation support is built-in, do BODY."
  (declare (indent 0))
  `(if-native-comp% (progn% ,@body)))

(defmacro v-home%> (&optional file)
  "Return the \\=`v-home*\\=' FILE with the suffix of compiled file."
  (concat (v-home* file) (if-native-comp% ".eln" ".elc")))

(defmacro v-comp-file! (src)
  "Make a versioned copy of SRC."
  (let ((s1 (gensym*)) (d1 (gensym*)) (d2 (gensym*)))
    `(let ((,s1 ,src))
       (when (and (stringp ,s1) (file-exists-p ,s1))
         (let* ((,d1 (v-path* ,s1))
                (,d2 (file-name-new-extension
                      ,d1
                      (if-native-comp% ".eln" ".elc"))))
           (when (file-newer-than-file-p ,s1 ,d1)
             (path! ,d1)
             (when (file-exists-p ,d2) (delete-file ,d2))
             (copy-file ,s1 ,d1 t))
           (cons ,d1 ,d2))))))

(defmacro compile-and-load-file* (src dst &optional only-compile)
  "Compile SRC to DST.\n
If ONLY-COMPILE is t, does not load DST."
  (let ((s1 (gensym*))
        (d1 (gensym*))
        (c1 (gensym*)))
    `(let* ((,s1 ,src)
            (,d1 ,dst)
            (,c1 ,only-compile))
       (unless (file-exists-p ,d1)
         (if-native-comp%
             (native-compile ,s1 ,d1)
           (byte-compile-file ,s1)))
       (cond (,c1 ,d1)
             (t (if-native-comp%
                    (native-elisp-load ,d1)
                  (load ,d1)))))))

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
                   (directory-files d nil "\\.el[cn]?\\'"))))
        (message "#Clean compiled files: %s..." d)
        (while fs
          (delete-file (concat d (car fs)))
          (setq fs (cdr fs))))
      (setq dirs (cdr dirs)))))


;; end of compile macro


;;; *-package% macro

(defmacro when-package% (&rest body)
  "If Emacs support package, do BODY."
  (declare (indent 0))
  `(when-version% <= 24.1 ,@body))

;; end of *-package% macro


;;; Boot

(when-native-comp%

  ;; slient native-comp warning
  (setq native-comp-async-report-warnings-errors 'silent)

  ;; first native-comp load
  (setcar native-comp-eln-load-path (v-home! ".eln/")))

;; boot
(let* ((u (v-comp-file! (emacs-home* "config/boot.el"))))
  (compile-and-load-file* (car u) (cdr u)))

;; package
(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;;; After loaded ...


;; end of init.el
