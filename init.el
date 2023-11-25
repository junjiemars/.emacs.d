;;; init.el --- init -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; Commentary: common notions.
;;;;

;;; compile-time macro

(defmacro nore-emacs ()
  "Nore Emacs git repo."
  "https://github.com/junjiemars/.emacs.d")

(defmacro comment (&rest body)
  "Ignore BODY, yields nil."
  nil)

(defmacro progn% (&rest body)
  "Return an \\=`progn\\='ed form if BODY has more than one sexp.\n
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


(unless% (fboundp 'gensym)
  (defvar gensym-counter 0 "The counter of \\=`gensym\\='.")
  (defun gensym (&optional prefix)
    "Generate a new uninterned symbol, PREFIX default is \"g\"."
    (make-symbol (format "%s%d" (or prefix "g")
                         (prog1 gensym-counter
                           (setq gensym-counter
                                 (1+ gensym-counter)))))))


(defmacro time (&rest form)
  "Return the elapsed time of FORM executing."
  (declare (indent 0))
  `(let ((b (current-time)))
     (prog1 (progn ,@form)
       (message "%.6f"
                (float-time (time-subtract (current-time) b))))))

;;; file macro

(defmacro emacs-home* (&optional file)
  "Return path of FILE under \\=`emacs-home\\='."
  `(concat ,(expand-file-name
             (or (getenv "EMACS_HOME") "~/.emacs.d/"))
           ,file))

(defmacro path! (file)
  "Make and return the path of the FILE.\n
The FILE should be posix path, see \\=`path-separator\\='."
  (let ((f (gensym)) (d (gensym))
        (i (gensym)) (ds (gensym)))
    `(let* ((,f ,file))
       (if (file-exists-p ,f)
           ,f
         (let ((,d (file-name-directory ,f)))
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
           ,f)))))


;; end basic file macro


;;; *-version% macro

(defconst +emacs-version+ (string-to-number emacs-version)
  "The version of Emacs in \\=`float\\='.")

(defmacro if-version% (cmp version then &rest else)
  "If VERSION CMP with variable \\=`emacs-version\\=' is t do THEN,
else do ELSE...\n
Return the value of THEN or the value of the last of the ELSE’s.
THEN must be one expression, but ELSE... can be zero or more
expressions.  If (CMP VERSION \\=`emacs-version\\=') yield nil,
and there are no ELSE’s, the value is nil."
  (declare (indent 3))
  `(if% (,cmp ,version +emacs-version+)
       ,then
     (progn% ,@else)))

(defmacro when-version% (cmp version &rest body)
  "When VERSION CMP with variable \\=`emacs-version\\=' yield
non-nil, do BODY."
  (declare (indent 2))
  `(if-version% ,cmp ,version (progn% ,@body)))

(defmacro v-name ()
  "Return the versioned name."
  `(concat (if (display-graphic-p) "g_" "t_")
           (number-to-string +emacs-version+)))

;; end of *-version% macro


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

(defmacro comp-file-extension% ()
  "Return extension of compiled file."
  `(if-native-comp% ".eln" ".elc"))

;; end of compile macro

;;; versioned file macro

(defun strrchr (str chr)
  "Return the index of the located CHR of STR from right side."
  (let* ((l (length str)) (i (1- l)))
    (catch 'break
      (while (>= i 0)
        (when (= chr (aref str i))
          (throw 'break i))
        (setq i (1- i))))))

(defun file-name-sans-extension* (file)
  "Return the FILE sans EXTENSION.\n
See \\=`file-name-sans-extension\\='."
  (let ((l (length file)))
    (when (> l 0)
      (let ((i (1- l)))
        (substring-no-properties
         file 0
         (catch 'break
           (while (>= i 0)
             (let ((c (aref file i)))
               (cond ((= ?/ c) (throw 'break l))
                     ((= ?. c) (throw 'break i))
                     (t (setq i (1- i))))))))))))

(defmacro v-path* (file)
  "Return versioned FILE."
  (let ((f1 (gensym)))
    `(let ((,f1 ,file))
       (concat (file-name-directory ,f1)
               ,(v-name) "/"
               (file-name-nondirectory ,f1)))))

(defmacro v-home* (file)
  "Return versioned FILE under \\=`emacs-home*\\='."
  `(v-path* (emacs-home* ,file)))

(defmacro v-home% (file)
  "Return versioned path of FILE under \\=`v-home*\\=' at compile-time."
  (v-home* file))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home*\\=' at compile-time."
  (path! (v-home* file)))

(defmacro v-home%> (file)
  "Return the \\=`v-home*\\=' FILE with the extension of compiled file."
  (concat (file-name-sans-extension* (v-home* file))
          (comp-file-extension%)))

;; end of versioned file macro

;;; compiler macro

(defmacro v-comp-file! (src)
  "Make a versioned copy of SRC."
  (let ((s1 (gensym)))
    `(let ((,s1 ,src))
       (when (and (stringp ,s1) (file-exists-p ,s1))
         (let* ((d1 (v-path* ,s1))
                (d2 (concat (file-name-sans-extension* d1)
                            ,(comp-file-extension%))))
           (when (file-newer-than-file-p ,s1 d1)
             (if (file-exists-p d2)
                 (delete-file d2)
               (path! d1))
             (copy-file ,s1 d1 t))
           (cons d1 d2))))))


(defmacro compile-and-load-file* (src dst &optional only-compile)
  "Compile SRC to DST.\n
If ONLY-COMPILE is t, does not load DST."
  (let ((s1 (gensym)) (d1 (gensym)) (c1 (gensym)))
    `(let* ((,s1 ,src) (,d1 ,dst) (,c1 ,only-compile))
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
  (let ((dirs (list `(,(v-home% "config/") . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% "private/") . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% "theme/") . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% ".exec/") . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(emacs-home*) . "[_a-z]+\\.el[cn]+\\'"))))
    (while dirs
      (let* ((d (car dirs)) (f1 (car d)) (r1 (cdr d))
             (fs (when (file-exists-p f1)
                   (directory-files f1 nil r1))))
        (message "#Clean compiled files: %s..." f1)
        (while fs
          (delete-file (concat f1 (car fs)))
          (setq fs (cdr fs))))
      (setq dirs (cdr dirs)))))


;; end of compiler macro


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
  (setcar native-comp-eln-load-path (v-home! ".eln/"))

  ;; env
  (when% (eq system-type 'darwin)
    (defun library-path ()
      "Even a blind pig can find an acorn once in a while."
      (let* ((arch (when (string-match
                          "\\-arch \\([_a-z0-9]+\\)"
                          system-configuration-options)
                     (substring-no-properties
                      system-configuration-options
                      (match-beginning 1) (match-end 1))))
             (platform (when (string-match
                              "\\(\\-.*?\\)\\."
                              system-configuration)
                         (concat arch
                                 (substring-no-properties
                                  system-configuration
                                  (match-beginning 1) (match-end 1)))))
             (c3 (when (string-match
                        "-rpath \\([/a-z]+/\\([a-z]+\\)\\([0-9]+\\)\\)"
                        system-configuration-options)
                   (let ((rpath (substring-no-properties
                                 system-configuration-options
                                 (match-beginning 1) (match-end 1)))
                         (cc (substring-no-properties
                              system-configuration-options
                              (match-beginning 2) (match-end 2)))
                         (ver (substring-no-properties
                               system-configuration-options
                               (match-beginning 3) (match-end 3))))
                     (cons rpath (cons cc ver)))))
             (path (concat (car c3) "/" (cadr c3) "/" platform))
             (fs (directory-files path nil (concat (cddr c3) "[.0-9]+"))))
        (concat path "/" (car fs))))
    (setenv "LIBRARY_PATH" (library-path))))

;; boot
(let ((u (v-comp-file! (emacs-home* "config/boot.el")))
      (gc-cons-percentage 0.4))
  (compile-and-load-file* (car u) (cdr u)))

;; package
(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;;; After loaded ...


;; end of init.el
