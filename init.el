;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; init.el
;;;;
;; Commentary: definitions.
;;;;

;;;
;; compile-time macro
;;;

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

(defvar *gensym-counter* 0 "The counter of \\=`gensym*\\='.")
(defmacro gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"n\"."
  `(make-symbol
    (format "%s%d" (or ,prefix "n")
            (prog1 *gensym-counter*
              (setq *gensym-counter* (1+ *gensym-counter*))))))

(defmacro inhibit-file-name-handler (&rest body)
  "Inhibit BODY under \\=`file-name-handler-alist\\='."
  (declare (indent 0))
  `(let ((file-name-handler-alist nil))
     ,@body))

(defmacro inhibit-gc (&rest body)
  "Inhibiti BODY under GC."
  (declare (indent 0))
  `(let ((gc-cons-percentage 0.4))
     ,@body))

;; end of compile-time macro

;;;
;; *-version% macro
;;;

(defconst +emacs-version+ (string-to-number emacs-version)
  "The \\=`float\\=' version of Emacs in.")

(defmacro if-version% (cmp version then &rest else)
  "If VERSION CMP with \\=`+emacs-version+\\=' yield non-nil, do
THEN, else do ELSE..."
  (declare (indent 3))
  `(if% (,cmp ,version +emacs-version+)
       ,then
     (progn% ,@else)))

(defmacro when-version% (cmp version &rest body)
  "When VERSION CMP with variable \\=`+emacs-version+\\=' yield
non-nil, do BODY."
  (declare (indent 2))
  `(if-version% ,cmp ,version (progn% ,@body)))

(defmacro v-name ()
  "Return the versioned name."
  `(concat (if (display-graphic-p) "g_" "t_")
           (number-to-string +emacs-version+)))

;; end of *-version% macro

;;;
;; file macro
;;;

(defun file-name-sans-extension* (file)
  "Return the FILE sans EXTENSION.\n
See \\=`file-name-sans-extension\\='."
  (let ((l (length file)))
    (when (> l 0)
      (let ((i (1- l)))
        (substring-no-properties
         file 0
         (catch 'br
           (while (>= i 0)
             (let ((c (aref file i)))
               (cond ((= ?/ c) (throw 'br l))
                     ((= ?. c) (throw 'br i))
                     (t (setq i (1- i))))))))))))

(defmacro path! (file)
  "Make and return the path of posixed FILE.\n"
  (let ((f (gensym*)) (d (gensym*)))
    `(inhibit-file-name-handler
       (let ((,f ,file))
         (if (file-exists-p ,f)
             ,f
           (let ((,d (file-name-directory ,f)))
             (prog1 ,f
               (unless (file-exists-p ,d)
                 (let ((i (1- (length ,d)))
                       (ds nil))
                   (catch 'br
                     (while (> i 0)
                       (when (= ?/ (aref ,d i))
                         (let ((s (substring-no-properties ,d 0 (1+ i))))
                           (if (file-exists-p s)
                               (throw 'br t)
                             (setq ds (cons s ds)))))
                       (setq i (1- i))))
                   (while (car ds)
                     (make-directory-internal (car ds))
                     (setq ds (cdr ds))))))))))))

;; end of file macro

;;;
;; versioned file macro
;;;

(defmacro emacs-home* (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\='."
  `(concat ,(expand-file-name
             (or (getenv-internal "EMACS_HOME") "~/.emacs.d/"))
           ,file))

(defmacro v-path (file)
  "Return versioned FILE."
  (let ((f1 (gensym*)))
    `(inhibit-file-name-handler
       (let ((,f1 ,file))
         (concat (file-name-directory ,f1)
                 ,(v-name) "/"
                 (file-name-nondirectory ,f1))))))

(defmacro v-home (&optional file)
  "Return versioned FILE under \\=`emacs-home*\\='."
  `(v-path (emacs-home* ,file)))

(defmacro v-home% (&optional file)
  "Return versioned path of FILE under \\=`v-home\\=' at
compile-time."
  (v-home file))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home\\=' at compile-time."
  (path! (v-home file)))

(defmacro v-home%> (file)
  "Return the \\=`v-home\\=' FILE with the extension of compiled file."
  (concat (file-name-sans-extension* (v-home file))
          (comp-file-extension%)))

;; end of versioned file macro

;;;
;; compile macro
;;;

(defmacro if-native-comp% (then &rest else)
  "If native compilation is built-in do THEN, else do ELSE..."
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

(defmacro v-comp-file! (src)
  "Make a versioned cons copy of SRC."
  (let ((s1 (gensym*)))
    `(let ((,s1 ,src))
       (let* ((d1 (v-path ,s1))
              (d2 (concat (file-name-sans-extension* d1)
                          ,(comp-file-extension%))))
         (when (file-newer-than-file-p ,s1 d1)
           (if (file-exists-p d2)
               (delete-file d2)
             (path! d1))
           (copy-file ,s1 d1 t))
         (cons d1 d2)))))

(defmacro time (id &rest form)
  "Run FORM and summarize resource usage."
  (declare (indent 0))
  `(let ((bt (float-time))
         (gc gcs-done)
         (gt gc-elapsed))
     (prog1 (progn% ,@form)
       (let ((ct (float-time)))
         (message "%.6f %d %.6f %.2f %d %d %d %d %d %d %d %d %.6f %s"
                  (- ct bt)
                  (- gcs-done gc)
                  (- gc-elapsed gt)
                  gc-cons-percentage
                  pure-bytes-used
                  cons-cells-consed
                  floats-consed
                  vector-cells-consed
                  symbols-consed
                  string-chars-consed
                  intervals-consed
                  strings-consed
                  ct
                  ,id)))))

(defmacro compile-and-load-file (src dst &optional only-compile)
  "Compile SRC to DST.\n
If ONLY-COMPILE is t, does not load DST."
  (let ((s1 (gensym*)) (d1 (gensym*)) (c1 (gensym*)))
    `(let ((,s1 ,src) (,d1 ,dst) (,c1 ,only-compile))
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
  (let ((dirs (list `(,(v-home% "config/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% "private/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% "theme/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home% ".exec/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(emacs-home*)
                      . "[_a-z]+\\.el[cn]+\\'"))))
    (while dirs
      (let* ((d (car dirs)) (f1 (car d)) (r1 (cdr d))
             (fs (when (file-exists-p f1)
                   (directory-files f1 nil r1))))
        (message "# Clean compiled files: %s..." f1)
        (while fs
          (delete-file (concat f1 (car fs)))
          (setq fs (cdr fs))))
      (setq dirs (cdr dirs)))))

;; end of compile macro

;;;
;; *-package% macro
;;;

(defmacro when-package% (&rest body)
  "If package is built-in, do BODY."
  (declare (indent 0))
  `(when-version% <= 24.1 ,@body))

;; end of *-package% macro

;;;
;; Boot
;;;

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
             (fs (let ((file-name-handler-alist nil))
                   (directory-files path nil
                                    (concat (cddr c3) "[.0-9]+") 1))))
        (concat path "/" (car fs))))
    (setq process-environment
          (cons (concat "LIBRARY_PATH=" (library-path))
                process-environment))))

(defmacro compile-and-load-file* (src dst &optional only-compile)
  "The profiling shim of \\=`compile-and-load-file*\\='."
  (if% (boundp '*nore-emacs-profile*)
      `(time ,src (compile-and-load-file ,src ,dst ,only-compile))
    `(compile-and-load-file ,src ,dst ,only-compile)))

;; boot
(unless (boundp '*nore-emacs-no-boot*)
  (inhibit-gc
    (inhibit-file-name-handler
      (let ((u (v-comp-file! (emacs-home* "config/boot.el"))))
        (compile-and-load-file* (car u) (cdr u))))))

;; end of Boot


;; package
(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;;; After loaded ...


;; end of init.el
