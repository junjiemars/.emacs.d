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
  "Ignore BODY, yield nil."
  nil)

(defmacro progn% (&rest body)
  "Return an \\=`progn\\='ed form if BODY has more than one sexp."
  (if (cdr body) `(progn ,@body) (car body)))

(defmacro if% (cond then &rest else)
  "If COND yield non-nil, do THEN, else do ELSE..."
  (declare (indent 2) (pure t))
  (if (funcall `(lambda () ,cond))
      `,then
    `(progn% ,@else)))

(defmacro when% (cond &rest body)
  "When COND yield non-nil, do BODY."
  (declare (indent 1))
  `(if% ,cond (progn% ,@body)))

(defmacro unless% (cond &rest body)
  "Unless COND yield nil, do BODY."
  (declare (indent 1))
  `(if% ,cond nil ,@body))

(defmacro inhibit-file-name-handler (&rest body)
  "Inhibit BODY under \\=`file-name-handler-alist\\='."
  (declare (indent 0))
  `(let ((file-name-handler-alist nil))
     ,@body))

(defmacro inhibit-gc (&rest body)
  "Inhibit BODY under GC."
  (declare (indent 0))
  `(let ((gc-cons-percentage 0.7))
     ,@body))

;; end of compile-time macro

;;;
;; *-version% macro
;;;

(defconst +emacs-version+ (string-to-number emacs-version)
  "The \\=`float\\=' version number of Emacs.")

(defmacro if-version% (cmp version then &rest else)
  "If VERSION CMP with \\=`+emacs-version+\\=' yield non-nil, do
THEN, else do ELSE..."
  (declare (indent 3) (pure t))
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

(defmacro file-name-sans-extension* (file)
  "Return the FILE sans extension."
  `(let* ((-fnse-f1- ,file)
          (l (length -fnse-f1-)))
     (when (> l 0)
       (let ((i (1- l)))
         (substring-no-properties
          -fnse-f1- 0
          (catch 'br
            (while (>= i 0)
              (let ((c (aref -fnse-f1- i)))
                (cond ((= ?/ c) (throw 'br l))
                      ((= ?. c) (throw 'br i))
                      (t (setq i (1- i))))))))))))

(defmacro mkdir* (file)
  "Make and return the path of posixed FILE."
  (declare (pure t))
  `(inhibit-file-name-handler
     (let ((-m-f1- ,file))
       (if (file-exists-p -m-f1-)
           -m-f1-
         (prog1 -m-f1-
           (let ((dir (file-name-directory -m-f1-)))
             (unless (file-exists-p dir)
               (let ((i (1- (length dir)))
                     (ds nil))
                 (catch 'br
                   (while (> i 0)
                     (when (= ?/ (aref dir i))
                       (let ((s (substring-no-properties dir 0 (1+ i))))
                         (if (file-exists-p s)
                             (throw 'br t)
                           (setq ds (cons s ds)))))
                     (setq i (1- i))))
                 (while (car ds)
                   (make-directory-internal (car ds))
                   (setq ds (cdr ds)))))))))))


;; end of file macro

;;;
;; versioned file macro
;;;

(defmacro emacs-home (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\='."
  (declare (pure t))
  `(concat ,(expand-file-name
             (or (getenv-internal "EMACS_HOME") "~/.emacs.d/"))
           ,file))

(defmacro v-path (file)
  "Return versioned FILE."
  (declare (pure t))
  `(inhibit-file-name-handler
     (let ((-vp-f1- ,file))
       (concat (file-name-directory -vp-f1-)
               ,(v-name) "/"
               (file-name-nondirectory -vp-f1-)))))

(defmacro v-home (&optional file)
  "Return versioned FILE under \\=`emacs-home\\='."
  `(v-path (emacs-home ,file)))

(defmacro make-v-home (file)
  "Make \\=`v-home\\='."
  `(mkdir* (v-home ,file)))

;; end of versioned file macro

;;;
;; compile macro
;;;

(defmacro if-native-comp% (then &rest else)
  "If native compilation is built-in do THEN, else do ELSE..."
  (declare (indent 1) (pure t))
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

(defmacro make-v-comp-file (src)
  "Make a versioned cons copy of SRC."
  `(inhibit-file-name-handler
     (let* ((-mvcf-s1- ,src)
            (-mvcf-d1- (v-path -mvcf-s1-))
            (-mvcf-d2- (concat (file-name-sans-extension* -mvcf-d1-)
                               (comp-file-extension%))))
       (when (file-newer-than-file-p -mvcf-s1- -mvcf-d1-)
         (if (file-exists-p -mvcf-d2-)
             (delete-file -mvcf-d2-)
           (mkdir* -mvcf-d1-))
         (copy-file -mvcf-s1- -mvcf-d1- t t))
       (cons -mvcf-d1- -mvcf-d2-))))

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
  `(let ((-calf-s1- ,src) (-calf-d1- ,dst) (-calf-c1- ,only-compile))
     (unless (file-exists-p -calf-d1-)
       (if-native-comp%
           (native-compile -calf-s1- -calf-d1-)
         (byte-compile-file -calf-s1-)))
     (cond (-calf-c1- -calf-d1-)
           (t (if-native-comp%
                  (native-elisp-load -calf-d1-)
                (load -calf-d1- nil nil t))))))

(defun clean-compiled-files ()
  "Clean all compiled files."
  (interactive)
  (let ((dirs (list `(,(v-home "config/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home "private/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home "theme/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(v-home ".exec/")
                      . "\\.el[cn]\\(\\.tmp\\)?\\'")
                    `(,(emacs-home)
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
  (declare (indent 0) (pure t))
  `(when-version% <= 24.1 ,@body))

;; end of *-package% macro

;;;
;; Boot
;;;

;; native-comp env
(when-native-comp%
  ;; slient native-comp warning
  (setq native-comp-async-report-warnings-errors 'silent)
  ;; first native-comp load
  (setcar native-comp-eln-load-path (make-v-home ".eln/"))
  ;; darwin native-comp env
  (when% (eq system-type 'darwin)
    (defun library-path ()
      "Even a blind pig can find an acorn once in a while."
      (let ((arch (when (string-match
                         "\\-arch \\([_a-z0-9]+\\)"
                         system-configuration-options)
                    (substring-no-properties
                     system-configuration-options
                     (match-beginning 1) (match-end 1))))
            (platform (when (string-match
                             "\\(\\-.*?\\)\\."
                             system-configuration)
                        (substring-no-properties
                         system-configuration
                         (match-beginning 1) (match-end 1))))
            (cc3 (when (string-match
                        "-rpath +\\([/a-z]+/\\([a-z]+\\)\\([0-9]+\\)\\)"
                        system-configuration-options)
                   (vector
                    ;; path
                    (substring-no-properties
                     system-configuration-options
                     (match-beginning 1) (match-end 1))
                    ;; cc
                    (substring-no-properties
                     system-configuration-options
                     (match-beginning 2) (match-end 2))
                    ;; ver
                    (substring-no-properties
                     system-configuration-options
                     (match-beginning 3) (match-end 3))))))
        (when (and arch platform cc3)
          (let ((path (concat
                       (aref cc3 0) "/" (aref cc3 1) "/"
                       (or (and (string-equal "arm64" arch)
                                (string-equal "gcc" (aref cc3 1))
                                (<= 14 (string-to-number (aref cc3 2)))
                                "aarch64")
                           arch)
                       platform)))
            (concat
             path "/"
             (car (inhibit-file-name-handler
                    (directory-files
                     path nil (concat (aref cc3 2) "[.0-9]+") 1))))))))
    (let ((libpath (library-path)))
      (when libpath
        (setq process-environment
              (cons (concat "LIBRARY_PATH=" libpath)
                    process-environment))))))

(defmacro compile-and-load-file* (src dst &optional only-compile)
  "Profiling shim of \\=`compile-and-load-file*\\='."
  (if% (boundp '*nore-emacs-profile*)
      `(time ,src (compile-and-load-file ,src ,dst ,only-compile))
    `(compile-and-load-file ,src ,dst ,only-compile)))

;; boot
(unless% (boundp '*nore-emacs-no-boot*)
  (inhibit-gc
    (let ((vc (make-v-comp-file (emacs-home "config/vcomp.el"))))
      (compile-and-load-file* (car vc) (cdr vc)))
    (compile! (compile-unit* (emacs-home* "config/fn.el")))
    (compile! (compile-unit* (emacs-home* "config/feature.el")))
    (compile! (compile-unit* (emacs-home* "config/boot.el")))))

;; end of Boot


;; package
(when-package%
  (setq package-enable-at-startup nil)
  (comment (package-initialize)))


;;; After loaded ...


;; end of init.el
