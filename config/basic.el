;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; basic.el
;;;;
;; Commentary: functional facilities.
;;;;

;;;
;; compatible macro
;;;

(unless-fn% 'user-error nil
  (defun user-error (format &rest args)
    "Signal a pilot error."
    (signal 'user-error
            (list (apply #'format-message format args)))))

(defmacro called-interactively-p* (&optional kind)
  "Return t if called by \\=`call-interactively\\='."
  (if-fn% 'called-interactively-p nil
          `(called-interactively-p ,kind)
    (ignore* kind)
    `(interactive-p)))

(unless-fn% 'with-eval-after-load nil
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
See \\=`with-eval-after-load\\='."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))

(defmacro defcustom% (symbol standard doc &rest args)
  "Declare SYMBOL as a customizable variable with the STANDARD value.
See \\=`defcustom\\='."
  (declare (doc-string 3) (debug (name body)))
  (let ((-standard- (funcall `(lambda () ,standard))))
    `(custom-declare-variable
      ',symbol
      ',-standard-
      ,doc
      ,@args)))

;;; `sxhash': see `%fn:save/read-sexp-to/from-file' in test.el
(define-hash-table-test 'string-hash= #'string= #'sxhash)

;; end of compatible function

;;;
;; *-region-active
;;;

(defmacro if-region-active (then &rest else)
  "If \\=`mark-active\\=' is non-nil, do THEN, else do ELSE..."
  (declare (indent 1))
  `(if mark-active
       ,then
     (progn% ,@else)))

(defmacro unless-region-active (&rest then)
  "Unless \\=`mark-active\\=' is non-nil, do THEN."
  (declare (indent 0))
  `(if-region-active nil ,@then))

;; end of *-region-active

;;;
;; file function
;;;

(defun path+ (root &rest path)
  "Append a list of PATH to ROOT."
  (declare (indent 1))
  (let* ((trim (lambda (x) (string-trim>< x "/" "/")))
         (tail (lambda (x) (concat (string-trim> x "/") "/")))
         (s (cond ((null root) (mapconcat trim path "/"))
                  ((null path) root)
                  (t (concat (funcall tail root)
                             (mapconcat trim path "/"))))))
    (if (string= "" s) nil (funcall tail s))))

(defmacro path- (file)
  "Return the parent path of FILE."
  (let ((f (gensym*)))
    `(let ((,f ,file))
       (when (stringp ,f)
         (inhibit-file-name-handler
           (file-name-directory (directory-file-name ,f)))))))

(defmacro path-depth (path &optional separator)
  "Return the depth of PATH."
  (let ((p (gensym*))
        (s (gensym*)))
    `(let ((,p ,path)
           (,s (or ,separator "/")))
       (if (= 0 (length ,p))
           0
         (- (length (split-string* ,p ,s nil)) 1)))))

(defmacro file-in-dirs-p (file dirs)
  "Return t if the name of FILE matching DIRS, otherwise nil."
  (let ((f (gensym*)) (ds (gensym*)))
    `(let ((,f ,file) (,ds ,dirs)
           (file-name-handler-alist nil))
       (when (and (stringp ,f) (consp ,ds))
         (some* (lambda (x)
                  (let ((case-fold-search (when-platform% 'windows-nt t)))
                    (and (stringp x)
                         (string-match x (file-name-directory ,f)))))
                ,ds)))))

(defmacro file-name-nondirectory% (filename)
  "Return file name FILENAME sans its directory at compile-time."
  (let* ((-f1- (funcall `(lambda () ,filename)))
         (-n1- (and -f1- (file-name-nondirectory -f1-))))
    `,-n1-))

(defun dir-iterate (dir ff df fn dn)
  "Iterate DIR.\n
FF file-filter (lambda (file-name absolute-name)...),
DF dir-filter (lambda (dir-name absolute-name)...),
FN file-processor (lambda (absolute-name)...),
DN dir-processor (lambda (aboslute-name)...)."
  (let* ((file-name-handler-alist nil)
         (files (remove-if* (lambda (x)
                              (or (null x)
                                  (string= "./" x)
                                  (string= "../" x)))
                            (file-name-all-completions "" dir))))
    (while files
      (let ((f (car files)))
        (let ((a (expand-file-name f dir)))
          (if (directory-name-p f)
              (when (and (let ((ln (file-symlink-p a)))
                           (if ln
                               (not (or
                                     (string-match "\\.\\'\\|\\.\\.\\'" ln)
                                     (and (>= (length a) (length ln))
                                          (string=
                                           ln
                                           (substring a 0 (length ln))))))
                             t))
                         df
                         (funcall df f a))
                (and dn (funcall dn a))
                (dir-iterate a ff df fn dn))
            (when (and ff (funcall ff f a))
              (and fn (funcall fn a)))))
        (setq files (cdr files))))))


(defun dir-backtrack (dir prefer)
  "Backtrack DIR.\n
Starting at DIR, look up directory hierarchy for prefered
directory or file. Ignores the symbol links of directory.\n
PREFER (lambda (dir files)...)."
  (let* ((file-name-handler-alist nil)
         (d (expand-file-name
             (if (directory-name-p dir)
                 dir
               (file-name-directory dir))))
         (stop "\\(^/\\|[a-zA-Z]:/\\)\\'"))
    (while (and (stringp d)
                (directory-name-p d)
                (not (string-match stop d)))
      (and prefer (funcall prefer d
                           (remove-if*
                            (lambda (x)
                              (or (null x)
                                  (string= "./" x)
                                  (string= "../" x)
                                  (let ((dx (concat d x)))
                                    (and (directory-name-p dx)
                                         (file-symlink-p dx)))))
                            (file-name-all-completions "" d))))
      (setq d (path- d)))))


(defmacro ssh-remote-p (file)
  "Return an identification when FILE specifies a location on a
remote system.\n
On ancient Emacs, \\=`file-remote-p\\=' will return a vector."
  `(string-match* "^\\(/sshx?:[_-a-zA-Z0-9]+@?[._-a-zA-Z0-9]+:\\)"
                  ,file 1))

(defmacro ssh-remote->ids (remote)
  "Norm the REMOTE to (method {user|id} [host]) form."
  (let ((r (gensym*)))
    `(let ((,r ,remote))
       (when (stringp ,r)
         (split-string* ,r "[:@]" t "\\(^/[^ssh].*$\\|^/\\)")))))

(defmacro ssh-remote->user@host (remote)
  "Norm the REMOTE to {user|id}[@host] form."
  (let ((rid (gensym*)))
    `(let ((,rid (ssh-remote->ids ,remote)))
       (when (consp ,rid)
         (concat (cadr ,rid)
                 (when (car (cddr ,rid))
                   (concat "@" (car (cddr ,rid)))))))))

;; end of file function

;;;
;; define key macro
;;;

(defmacro if-key% (keymap key test then &rest else)
  "If TEST yields t for KEY in KEYMAP do then, else do ELSE..."
  (declare (indent 3))
  `(if% (funcall ,test (lookup-key ,keymap ,key))
       ,then
     ,@else))

(defmacro define-key% (keymap key def)
  "Define KEY to DEF in KEYMAP."
  `(if-key% ,keymap ,key
            (lambda (d) (not (eq d ,def)))
     (define-key ,keymap ,key ,def)))

;; end of define key macro


(defmacro symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (let ((ss (gensym*)))
    `(if-region-active
         (let ((,ss (buffer-substring-no-properties
                     (region-beginning)
                     (region-end))))
           (setq mark-active nil)
           (cons 'region ,ss))
       (let ((,ss (thing-at-point (or ,thing 'symbol))))
         (and ,ss (cons nil (substring-no-properties ,ss)))))))


(defun newline* (&optional arg)
  "Raw newline."
  (interactive "*P")
  (let ((electric-indent-mode nil))
    (when-version% > 26
      (when-lexical% (ignore* electric-indent-mode)))
    (if-version% <= 24.4
                 (newline arg 'interactive)
      (newline arg))))


;; end of basic.el
