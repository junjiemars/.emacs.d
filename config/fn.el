;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; fn.el
;;;;
;; Commentary: common notions.
;;;;

;;;
;; alias
;;;

(fset 'range #'number-sequence)

(unless-fn% 'char= nil
  (fset 'char= #'char-equal))

(unless-fn% 'characterp nil
  (fset 'characterp #'char-valid-p))

;; end of alias

;;;
;; common-lisp macro
;;;

;; Load cl-lib/cl at runtime
(eval-when-compile
  (if-version%
      <= 24.1
      (require 'cl-lib)
    (require 'cl)))

(fset 'assoc**
      (if-fn% 'cl-assoc 'cl-lib
              #'cl-assoc
        #'assoc*))

(fset 'mapcar**
      (if-fn% 'cl-mapcar 'cl-lib
              #'cl-mapcar
        #'mapcar*))

(fset 'remove-if*
      (if-fn% 'cl-remove-if 'cl-lib
              #'cl-remove-if
        #'remove-if))

(fset 'member-if*
      (if-fn% 'cl-member-if 'cl-lib
              #'cl-member-if
        #'member-if))

(fset 'every*
      (if-fn% 'cl-every 'cl-lib
              #'cl-every
        #'every))

(fset 'some*
      (if-fn% 'cl-some 'cl-lib
              #'cl-some
        #'some))

(fset 'loop*
      (if-fn% 'cl-loop 'cl-lib
              #'cl-loop
        #'loop))

;; end of common-lisp macro

;;;
;; general function/macro
;;;

(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))

(unless-fn% 'take nil
  (defun take (n seq)
    "Return a sequence of the first N items in SEQ."
    (let ((s nil))
      (while (and (> n 0) seq)
        (setq s (cons (car seq) s)
              n (1- n)
              seq (cdr seq)))
      (nreverse s))))

(defun take-while (pred seq)
  "Return a sequence of items from SEQ just take while PRED is t."
  (let ((s nil))
    (while (and seq (not (funcall pred (car seq))))
      (setq s (cons (car seq) s)
            seq (cdr seq)))
    (nreverse s)))

(defmacro drop (n seq)
  "Return rest sequence after drop the first N items in SEQ."
  `(nthcdr ,n ,seq))

(defun drop-while (pred seq)
  "Return a sequence of items from SEQ drop while PRED is t."
  (while (and seq (funcall pred (car seq)))
    (setq seq (cdr seq)))
  seq)

(defmacro fluid-let (binding &rest body)
  "Execute BODY and restore the BINDING after return."
  (declare (indent 1))
  (let ((old (gensym*))
        (var (car binding))
        (new (gensym*)))
    `(let ((,old ,(car binding))
           (,new ,(cadr binding)))
       (prog1
           (unwind-protect (progn
                             (setq ,var ,new)
                             ,@body)
             (setq ,var ,old))
         (setq ,var ,old)))))

(defmacro push! (newelt seq &optional uniquely)
  "Push NEWELT to the head of SEQ.\n
If optional UNIQUELY is non-nil then push uniquely."
  (let ((n1 (gensym*)))
    `(let ((,n1 ,newelt))
       (setq ,seq (if ,uniquely
                      (cons ,n1 (delete ,n1 ,seq))
                    (cons ,n1 ,seq))))))

(defmacro append! (newelt seq &optional uniquely)
  "Append NEWELT to the end of SEQ.\n
If optional UNIQUELY is non-nil then append uniquely."
  (let ((n1 (gensym*)) (s1 (gensym*)))
    `(let ((,n1 ,newelt) (,s1 ,seq))
       (setq ,seq (nconc (if ,uniquely
                             (delete ,n1 ,s1)
                           ,s1)
                         (list ,n1))))))

(defmacro insert! (newelt seq idx)
  "Insert NEWELT into the SEQ."
  (let ((n1 (gensym*))
        (i1 (gensym*)))
    `(let ((,n1 ,newelt)
           (,i1 ,idx))
       (let ((l (length ,seq)))
         (when (and (integerp ,i1) (>= ,i1 0) (< ,i1 l))
           (let ((c1 (copy-sequence ,seq))
                 (j (1+ ,i1)))
             (while (< j l)
               (setf (nth j c1) (nth (1- j) ,seq) j (1+ j)))
             (setf (nth ,i1 c1) ,n1)
             (setq ,seq (append c1 `(,(nth (1- l) ,seq))))))))))

;; end of general function/macro

;;;
;; byte-compiler macro
;;;

(defmacro defmacro-if-feature% (feature)
  "Define if-feature-FEATURE% compile-time macro."
  (let ((ss (format "if-feature-%s%%" feature)))
    (unless (intern-soft ss)
      (let ((name (intern ss)))
        `(defmacro ,name (then &rest body)
           "If has the feauture do THEN, otherwise do BODY."
           (declare (indent 1))
           (if% (require ',feature nil t)
               `(progn% (comment ,@body)
                        ,then)
             `(progn% (comment ,then)
                      ,@body)))))))

(defmacro defmacro-if-fn% (fn &optional feature)
  "Define if-fn-FN% compile-time macro."
  (let ((ss (format "if-fn-%s%%" fn)))
    (unless (intern-soft ss)
      (let ((name (intern ss)))
        `(defmacro ,name (then &rest body)
           "If has the fn do THEN, otherwise do BODY."
           (declare (indent 1))
           (if-fn% ',fn ',feature
                   `(progn% (comment ,@body)
                            ,then)
             `(progn% (comment ,then)
                      ,@body)))))))

(defmacro make-thread* (fn &optional join name)
  "Threading call FN with NAME or in JOIN mode."
  `(if-fn% 'make-thread nil
           (if% ,join
               (thread-join (make-thread ,fn ,name))
             (make-thread ,fn ,name))
     (ignore* ,join ,name)
     (if% (functionp ,fn)
         (funcall ,fn)
       ,fn)))

;; end of byte-compiler macro

;;;
;; string
;;;

(defun strchr (str chr)
  "Return the index of the located CHR of STR from left side."
  (let ((i 0) (l (length str)))
    (catch 'br
      (while (< i l)
        (when (= chr (aref str i))
          (throw 'br i))
        (setq i (1+ i))))))

(defun strrchr (str chr)
  "Return the index of the located CHR of STR from right side."
  (let* ((l (length str)) (i (1- l)))
    (catch 'br
      (while (>= i 0)
        (when (= chr (aref str i))
          (throw 'br i))
        (setq i (1- i))))))

(defun string-trim> (s &optional rr)
  "Remove tailing whitespaces or matching of RR at the end of S."
  (when (stringp s)
    (let ((r1 (concat "\\(?:" (or rr "[ \t\n\r]+") "\\)\\'")))
      (let ((i (string-match r1 s 0)))
        (if i (substring-no-properties s 0 i) s)))))

(defun string-trim< (s &optional lr)
  "Remove leading whitespaces or matching of LR from S."
  (when (stringp s)
    (let ((r1 (concat "\\`\\(?:" (or lr "\\`[ \t\n\r]+") "\\)")))
      (if (string-match r1 s)
          (substring-no-properties s (match-end 0))
        s))))

(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespaces or matching of LR/RR from S."
  `(string-trim> (string-trim< ,s ,lr) ,rr))

(defmacro string-match* (regexp string num &optional start)
  "Return the NUMth match for REGEXP in STRING from START.\n
See \\=`string-match\\=' and \\=`match-string\\='."
  (let ((s (gensym*))
        (n (gensym*)))
    `(let* ((,s ,string)
            (,n ,num)
            (b (and (stringp ,s) (string-match ,regexp ,s ,start)
                    (match-beginning ,n))))
       (when b
         (substring-no-properties ,s b (match-end ,n))))))

(defmacro split-string* (string &optional separators omit-nulls trim)
  "Split STRING into substrings bounded by match for SEPARATORS.\n
Like \\=`split-string\\=' in Emacs 24.4+
Optional argument OMIT-NULLS omit null strings.
Optional argument TRIM regexp used to trim."
  (if-version%
      <= 24.4
      `(split-string ,string ,separators ,omit-nulls ,trim)
    (let ((s (gensym*))
          (p (gensym*))
          (d (gensym*)))
      `(let ((,s ,string)
             (,p ,separators)
             (,d ,trim))
         (if ,d
             (delete ""
                     (mapcar (lambda (s)
                               (if (and (stringp ,d) (> (length ,d) 0))
                                   (string-trim>< s ,d ,d)
                                 (string-trim>< s)))
                             (split-string ,s ,p ,omit-nulls)))
           (split-string ,s ,p ,omit-nulls))))))

;; end of string

;;;
;; read/save-str/sexp-file
;;;

(defmacro get-buffer-create*
    (buffer-or-name &optional inhibit-buffer-hooks)
  "See \\=`get-buffer-create\\='."
  (if-version%
      <= 28
      `(get-buffer-create ,buffer-or-name ,inhibit-buffer-hooks)
    `(if ,inhibit-buffer-hooks
         (lexical-let%
             ((kill-buffer-hook nil)
              (kill-buffer-query-functions nil)
              (buffer-list-update-hook nil))
           (get-buffer-create ,buffer-or-name))
       (get-buffer-create ,buffer-or-name))))

(defmacro insert-file-contents-literally*
    (filename &optional visit beg end replace)
  "See \\=`insert-file-contents-literally\\='."
  `(lexical-let%
       ((format-alist nil)
        (after-insert-file-functions nil)
        (file-coding-system-alist nil)
        (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion))
     (insert-file-contents ,filename ,visit ,beg ,end ,replace)))

(defmacro write-region*
    (start end filename &optional append visit lockname mustbenew)
  "See \\=`write-region\\='."
  `(lexical-let%
       ((format-alist nil)
        (coding-system-for-write 'no-conversion)
        (write-region-inhibit-fsync t)
        (write-region-annotate-functions nil))
     (write-region ,start ,end ,filename
                   ,append ,visit ,lockname ,mustbenew)))

(defmacro save-sexp-to-file (sexp file)
  "Save SEXP to FILE.\n
Returns the name of FILE when successed otherwise nil."
  (let ((s (gensym*)) (f (gensym*)))
    `(let ((,s ,sexp) (,f ,file)
           (b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
           (with-current-buffer b
             (prin1 ,s b)
             (write-region* (point-min) (point-max) ,f)
             ,f)
         (when b (kill-buffer b))))))

(defmacro read-sexp-from-file (file)
  "Read the first sexp from FILE."
  (let ((f (gensym*)))
    `(lexical-let% ((,f ,file))
       (when (and (stringp ,f) (file-exists-p ,f))
         (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
           (unwind-protect
               (with-current-buffer b
                 (insert-file-contents-literally* ,f)
                 (read b))
             (when b (kill-buffer b))))))))

(defmacro save-str-to-file (str file)
  "Save STR to FILE.\n
Returns the name of FILE when successed otherwise nil."
  (let ((s (gensym*)) (f (gensym*)))
    `(lexical-let%
         ((,s ,str) (,f ,file)
          (b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
           (with-current-buffer b
             (insert ,s)
             (write-region* (point-min) (point-max) ,f nil :slient)
             ,f)
         (when b (kill-buffer b))))))

(defmacro read-str-from-file (file)
  "Read string from FILE."
  (let ((f (gensym*)))
    `(lexical-let% ((,f ,file))
       (when (and (stringp ,f) (file-exists-p ,f))
         (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
           (unwind-protect
               (with-current-buffer b
                 (insert-file-contents-literally* ,f)
                 (buffer-substring-no-properties (point-min) (point-max)))
             (when b (kill-buffer b))))))))

;; end of read/save-str/sexp-file

;;;
;; platform macro
;;;

(defmacro file-name-base* (path)
  "Return base name of PATH."
  (let ((p (gensym*)))
    `(let* ((,p ,path))
       (substring-no-properties
        ,p
        (let ((p1 (strrchr ,p ?/)))
          (if p1 (1+ p1) 0))
        (or (strrchr ,p ?.) (length ,p))))))

(unless% (fboundp 'directory-name-p)
  (defmacro directory-name-p (name)
    "Return t if NAME ends with a directory separator character."
    (let ((n (gensym*))
          (w (gensym*)))
      `(let* ((,n ,name)
              (,w (length ,n)))
         (and (> ,w 0) (= ?/ (aref ,n (1- ,w))))))))

(defmacro posix-path (path)
  "Transpose PATH to posix path."
  (let ((p (gensym*)))
    `(let ((,p ,path))
       (when (stringp ,p)
         (if (string-match "^\\([A-Z]:\\)" ,p)
             (replace-regexp-in-string
              "\\\\"
              "/"
              (replace-match
               (downcase (match-string 1 ,p))
               t t ,p))
           ,p)))))

(defmacro shell-command* (command &rest args)
  "Return a cons cell (code . output) after execute COMMAND in
 inferior shell.\n
See \\=`shell-command\\=' and \\=`shell-command-to-string\\=' for
details. If you want to set the environment temporarily that
\\=`shell-command*\\=' run in:
 (let ((process-environment (cons \"XXX=zzz\" process-environment)))
   (shell-command* \"echo $XXX\"))\n
Optional argument ARGS for COMMAND."
  (declare (indent 1))
  (let ((c1 (gensym*)))
    `(lexical-let%
         ((,c1 ,command)
          (b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
         (with-current-buffer b
           (cons
            (let ((x (call-process
                      shell-file-name nil b nil
                      shell-command-switch
                      (mapconcat #'identity
                                 (list ,c1 ,@args)
                                 " "))))
              (cond ((integerp x) x)
                    ((string-match "^.*\\([0-9]+\\).*$" x)
                     (match-string-no-properties 1 x))
                    (t -1)))
            (let ((s (buffer-substring-no-properties
                      (point-min) (point-max))))
              (if (string= "\n" s) nil s))))
         (when b (kill-buffer b))))))

(defmacro executable-find% (command &optional fn)
  "Return the path of COMMAND at compile time.\n
Call FN with the path if FN is non-nil."
  (let ((cmd (shell-command* (if-platform% 'windows-nt
                                 "where"
                               "command -v")
               (funcall `(lambda () ,command)))))
    (when (zerop (car cmd))
      (let* ((ss (cdr cmd))
             (ps (string-trim> ss "\n"))
             (path (if fn
                       (funcall fn (shell-quote-argument ps))
                     (if-platform% 'windows-nt
                         (posix-path ps)
                       ps))))
        `,path))))

(defmacro emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (if (= most-positive-fixnum (1- (expt 2 61))) 64
    (if (= most-positive-fixnum (1- (expt 2 29))) 32 0)))

(defmacro platform-arch ()
  "Return platform architecture."
  (let ((m64 "\\([xX]86_64\\|[aA][mM][dD]64\\|aarch64\\|arm64\\)"))
    (if (string-match m64 system-configuration)
        (string-match* m64 system-configuration 1)
      (if-platform% 'windows-nt
          (if (string-match m64 (getenv-internal "PROCESSOR_ARCHITECTURE"))
              (string-match*
                m64 (getenv-internal "PROCESSOR_ARCHITECTURE") 1)
            (getenv-internal "PROCESSOR_ARCHITECTURE"))
        (let ((m (shell-command* "uname" "-m")))
          (if (and (zerop (car m))
                   (string-match m64 (string-trim> (cdr m) "\n")))
              (string-trim> (cdr m) "\n")
            (string-trim> (cdr m) "\n")))))))

(defun version-string= (v1 v2)
  "Return 0 if V1 equals V2, -1 if V1 less than V2, otherwise 1."
  (let ((l1 (length v1)) (l2 (length v2))
        (nv1 0) (nv2 0)
        (i 0) (j1 0) (j2 0) (k1 0) (k2 0))
    (cond ((and (= l1 0) (= l2 0)) 0)
          ((and (= l1 0) (> l2 0)) -1)
          ((and (> l1 0) (= l2 0) 1))
          (t (catch 'br
               (while (< i 4)
                 (setq nv1
                       (catch 'br1
                         (when (= j1 l1) (throw 'br1 0))
                         (while (< j1 l1)
                           (when (= ?. (aref v1 j1))
                             (throw 'br1
                                    (string-to-number
                                     (substring-no-properties
                                      v1 k1 (prog1 j1
                                              (setq j1 (1+ j1)
                                                    k1 j1))))))
                           (setq j1 (1+ j1)))
                         (string-to-number
                          (substring-no-properties v1 k1 j1)))
                       nv2
                       (catch 'br2
                         (when (= j2 l2) (throw 'br2 0))
                         (while (< j2 l2)
                           (when (= ?. (aref v2 j2))
                             (throw 'br2
                                    (string-to-number
                                     (substring-no-properties
                                      v2 k2 (prog1 j2
                                              (setq j2 (1+ j2)
                                                    k2 j2))))))
                           (setq j2 (1+ j2)))
                         (string-to-number
                          (substring-no-properties v2 k2 j2))))
                 (cond ((< nv1 nv2) (throw 'br -1))
                       ((> nv1 nv2) (throw 'br 1))
                       ((and (= j1 l1) (= j2 l2)) (throw 'br 0))
                       (t (setq i (1+ i))))))))))

;; end of platform macro

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
    "Execute BODY after FILE is loaded."
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

;; end of compatible function

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

(defun file-in-dirs-p (file dirs)
  "Return the matched dir if FILE in DIRS, otherwise nil."
  (when (and (stringp file) (consp dirs))
    (inhibit-file-name-handler
      (let ((case-fold-search (when-platform% 'windows-nt t))
            (d (file-name-directory file)))
        (catch 'br
          (dolist* (x dirs)
            (when (and (stringp x)
                       (eq 't
                           (compare-strings
                            x 0 (length x) d 0 (length x)
                            case-fold-search)))
              (throw 'br x))))))))

(defmacro file-name-nondirectory% (filename)
  "Return file name FILENAME sans its directory at compile-time."
  (let* ((-f1- (funcall `(lambda () ,filename)))
         (-n1- (and -f1- (file-name-nondirectory -f1-))))
    `,-n1-))

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

;;;
;; interactive fn/macro
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

(unless-fn% 'delete-line nil
  (defun delete-line ()
    "Delete current line."
    (let ((inhibit-field-text-motion t))
      (delete-region (line-beginning-position)
                     (line-beginning-position 2)))))

 ;; end of interactive fn/macro

(provide 'fn)

;; end of fn.el
