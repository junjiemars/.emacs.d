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
;; compiled init fn
;;;

(defun path! (file)
  (mkdir! file))

(defun v-comp-file! (src)
  (make-v-comp-file! src))

;; end of compiled init fn


;;;
;; *-fn%: checking fn existing
;;;

(defmacro if-fn% (fn feature then &rest else)
  "If FN is bounded yield non-nil, do THEN, else do ELSE...\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 3) (pure t))
  `(if% (cond ((null ,feature) (fboundp ,fn))
              (t (and (require ,feature nil t)
                      (fboundp ,fn))))
       ,then
     (progn% ,@else)))

(defmacro when-fn% (fn feature &rest body)
  "When FN is bounded yield non-nil, do BODY.\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature (progn% ,@body)))

(defmacro unless-fn% (fn feature &rest body)
  "Unless FN is bounded yield non-nil, do BODY.\n
Argument FEATURE that FN dependent on, be loaded at compile time."
  (declare (indent 2))
  `(if-fn% ,fn ,feature nil ,@body))

(unless-fn% 'declare-function nil
  (defmacro declare-function (&rest _)))

;; end of *-fn% macro


;;;
;; *-var%: checking var existing
;;;

(defmacro if-var% (var feature then &rest else)
  "If VAR is bounded yield non-nil, do THEN, else do ELSE...\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 3) (pure t))
  `(if% (or (and ,feature (require ,feature nil t) (boundp ',var))
            (boundp ',var))
       ,then
     (progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "When VAR is bounded yield non-nil, do BODY.\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature (progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Unless VAR is bounded yield non-nil, do BODY.\n
Argument FEATURE that VAR dependent on, load at compile time."
  (declare (indent 2))
  `(if-var% ,var ,feature nil ,@body))

(defmacro setq% (x val &optional feature)
  "Set X to the value of VAL when X is bound.\n
Argument FEATURE that X dependent on, load at compile time."
  ;; (declare (debug t))
  `(when-var% ,x ,feature
     (setq ,x ,val)))

;; end of *-var% macro


;;;
;; *lexical*: checking lexical context
;;;

(defmacro if-lexical% (then &rest else)
  "If lexical binding is built-in do THEN, otherwise do ELSE..."
  (declare (indent 1) (pure t))
  `(if-version%
       <= 24.1
       ,then
     (progn% ,@else)))

(defmacro when-lexical% (&rest body)
  "When lexical binding is built-in do BODY."
  (declare (indent 0))
  `(if-lexical% (progn% ,@body)))

(defmacro unless-lexical% (&rest body)
  "Unless lexical binding is built-in do BODY."
  (declare (indent 0))
  `(if-lexical% nil ,@body))

(defmacro lexical-let% (varlist &rest body)
  "Lexically bind VARLIST in parallel then eval BODY."
  (declare (indent 1))
  `(if-lexical%
       (if-version% < 27
                    (let ,varlist ,@body)
         (let ((lexical-binding t))
           (let ,varlist ,@body)))
     (when-fn% 'lexical-let 'cl
       ;; `lexical-let' since Emacs22
       (lexical-let ,varlist ,@body))))

(defmacro lexical-let*% (varlist &rest body)
  "Lexically bind VARLIST sequentially then eval BODY."
  (declare (indent 1))
  `(if-lexical%
       (if-version% < 27
                    (let* ,varlist ,@body)
         (let ((lexical-binding t))
           (let* ,varlist ,@body)))
     (when-fn% 'lexical-let* 'cl
       ;; `lexical-let' since Emacs22
       (lexical-let* ,varlist ,@body))))

(defmacro ignore* (&rest vars)
  "Return nil, list VARS at compile time if in lexical context."
  (declare (indent 0))
  (when-lexical%
    (list 'prog1 nil (cons 'list `,@vars))))

(defun true (&rest x)
  "Return true value ignore X."
  (prog1 t (ignore* x)))

(defmacro safe-local-variable* (var &optional fn)
  "Safe local VAR with FN."
  `(put ,var 'safe-local-variable (or ,fn #'true)))

;; end of *lexical* compile-time macro

;;;
;; *-graphic%: checking graphical context
;;;

(defmacro if-graphic% (then &rest else)
  "If \\=`display-graphic-p\\=' yield non-nil, do THEN, else do ELSE..."
  (declare (indent 1) (pure t))
  (if (display-graphic-p)
      `,then
    `(progn% ,@else)))

(defmacro when-graphic% (&rest body)
  "When \\=`display-graphic-p\\=' yield non-nil, do BODY."
  (declare (indent 0))
  `(if-graphic% (progn% ,@body)))

(defmacro unless-graphic% (&rest body)
  "Unless \\=`display-graphic-p\\=' yield nil, do BODY."
  (declare (indent 0))
  `(if-graphic% nil ,@body))

;; end of *-graphic% macro

;;;
;; *-platform%: checking platform identifier
;;;

(defmacro if-platform% (os then &rest else)
  "If OS eq \\=`system-type\\=' yield non-nil, do THEN, else do ELSE..."
  (declare (indent 2) (pure t))
  `(if% (eq system-type ,os)
       ,then
     (progn% ,@else)))

(defmacro when-platform% (os &rest body)
  "When OS eq \\=`system-type\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-platform% ,os (progn% ,@body)))

(defmacro unless-platform% (os &rest body)
  "Unless OS eq \\=`system-type\\=' yield non-nil do BODY."
  (declare (indent 1))
  `(if-platform% ,os nil ,@body))

;; end of *-platform% macro

;;;
;; *-window%: checking windowing identifier
;;;

(defmacro if-window% (window then &rest else)
  "If WINDOW eq \\=`initial-window-system\\=' yield non-nil, do THEN,
else do ELSE..."
  (declare (indent 2) (pure t))
  `(if% (eq initial-window-system ,window)
       ,then
     (progn% ,@else)))

(defmacro when-window% (window &rest body)
  "When WINDOW eq \\=`initial-window-system\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-window% ,window (progn% ,@body)))

(defmacro unless-window% (window &rest body)
  "Unless WINDOW eq \\=`initial-window-system\\=' yield non-nil, do BODY."
  (declare (indent 1))
  `(if-window% ,window nil ,@body))

;; end of *-window% macro

;;;
;; *-noninteractive%: checking non-interactive context
;;;

(defmacro if-noninteractive% (then &rest body)
  "If \\=`noninteractive\\=' do THEN, else do BODY."
  (declare (indent 1) (pure t))
  `(if% noninteractive
       ,then
     (progn% ,@body)))

(defmacro unless-noninteractive% (&rest body)
  "Unless \\=`noninteractive\\=' do BODY."
  (declare (indent 0))
  `(if-noninteractive% nil ,@body))

;; end of *-noninteractive% macro

;;;
;; preferred lexical `dolist*'
;;;

(defmacro dolist* (spec &rest body)
  "Loop over a list and do DOBY.\n
Lexically \\=`do-list\\='.
Argument SPEC (VAR LIST [RESULT])."
  (declare (indent 1) (debug ((symbolp form &optional form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (unless (and (<= 2 (length spec)) (<= (length spec) 3))
    (signal 'wrong-number-of-arguments (list '(2 . 3) (length spec))))
  (let ((lst (gensym*)))
    `(lexical-let% ((,lst ,(nth 1 spec)))
       (while ,lst
         (let ((,(car spec) (car ,lst)))
           ,@body
           (setq ,lst (cdr ,lst))))
       ,@(cdr (cdr spec)))))

;; end of preferred `dolist*'

;;;
;; feature
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

;; end of feature

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
;; sequence
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

;; end of sequence

;;;
;; thread
;;;

(defmacro make-thread* (fn &optional join name)
  "Threading call FN with NAME or in JOIN mode."
  `(if-fn% 'make-thread nil
           (if% ,join
               (thread-join (make-thread ,fn ,name))
             (make-thread ,fn ,name))
     (ignore* ,join ,name)
     (funcall ,fn)))

(defmacro thread-yield* ()
  "Yield the CPU to another thread."
  `(when-fn% 'thread-yield nil (thread-yield)))

;; end of thread

;;;
;; string
;;;

(defun strchr (str chr)
  "Return the index of the located CHR of STR from left side."
  (let ((i 0) (l (length str)))
    (catch 'br
      (while (< i l)
        (and (= chr (aref str i)) (throw 'br i))
        (setq i (1+ i))))))

(defun strrchr (str chr)
  "Return the index of the located CHR of STR from right side."
  (let* ((l (length str)) (i (1- l)))
    (catch 'br
      (while (>= i 0)
        (and (= chr (aref str i)) (throw 'br i))
        (setq i (1- i))))))

(defun string-trim> (s &optional rr)
  "Remove tailing whitespaces or matching of RR at the end of S."
  (and (stringp s)
       (let ((r1 (concat "\\(?:" (or rr "[ \t\n\r]+") "\\)\\'")))
         (let ((i (string-match r1 s 0)))
           (if i (substring-no-properties s 0 i) s)))))

(defun string-trim< (s &optional lr)
  "Remove leading whitespaces or matching of LR from S."
  (and (stringp s)
       (let ((r1 (concat "\\`\\(?:" (or lr "\\`[ \t\n\r]+") "\\)")))
         (if (string-match r1 s)
             (substring-no-properties s (match-end 0))
           s))))

(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespaces or matching of LR/RR from S."
  (declare (pure t))
  `(string-trim> (string-trim< ,s ,lr) ,rr))

(defun string-match* (regexp string num &optional start)
  "Return the NUMth match for REGEXP in STRING from START.\n
See \\=`string-match\\=' and \\=`match-string\\='."
  (let ((b (and (stringp string) (string-match regexp string start)
                (match-beginning num))))
    (and b (substring-no-properties string b (match-end num)))))

(when-version% > 24.4
  (defun split-string4 (string &optional separators omit-nulls trim)
    (if trim
        (delete ""
                (mapcar (lambda (s)
                          (if (and (stringp trim) (> (length trim) 0))
                              (string-trim>< s trim trim)
                            (string-trim>< s)))
                        (split-string string separators omit-nulls)))
      (split-string string separators omit-nulls))))

(defmacro split-string* (string &optional separators omit-nulls trim)
  "Split STRING into substrings bounded by match for SEPARATORS.\n
Optional argument OMIT-NULLS omit null strings.
Optional argument TRIM regexp used to trim."
  (declare (pure t))
  (if-version%
      <= 24.4
      `(split-string ,string ,separators ,omit-nulls ,trim)
    `(split-string4 ,string ,separators ,omit-nulls ,trim)))

;; end of string

;;;
;; IO
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

(defun save-sexp-to-file (sexp file)
  "Save SEXP to FILE.\n
Returns the name of FILE when successed otherwise nil."
  (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
    (unwind-protect
        (with-current-buffer b
          (prin1 sexp b)
          (write-region* (point-min) (point-max) file)
          file)
      (and b (kill-buffer b)))))

(defun read-sexp-from-file (file)
  "Read the first sexp from FILE."
  (when (and (stringp file) (file-exists-p file))
    (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
      (unwind-protect
          (with-current-buffer b
            (insert-file-contents-literally* file)
            (read b))
        (and b (kill-buffer b))))))

(defun save-str-to-file (str file)
  "Save STR to FILE.\n
Return the name of FILE when successed otherwise nil."
  (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
    (unwind-protect
        (with-current-buffer b
          (insert str)
          (write-region* (point-min) (point-max) file nil :slient)
          file)
      (and b (kill-buffer b)))))

(defun read-str-from-file (file)
  "Read string from FILE."
  (when (and (stringp file) (file-exists-p file))
    (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
      (unwind-protect
          (with-current-buffer b
            (insert-file-contents-literally* file)
            (buffer-substring-no-properties (point-min) (point-max)))
        (and b (kill-buffer b))))))

;; end of IO

;;;
;; platform
;;;

(defun file-name-base* (path)
  "Return base name of PATH."
  (substring-no-properties path
                           (let ((p1 (strrchr path ?/)))
                             (if p1 (1+ p1) 0))
                           (or (strrchr path ?.) (length path))))

(unless% (fboundp 'directory-name-p)
  (defun directory-name-p (name)
    "Return t if NAME ends with a directory separator character."
    (let ((len (length name)))
      (and (> len 0) (= ?/ (aref name (1- len)))))))

(defun posix-path (path)
  "Transpose PATH to posix path."
  (when (stringp path)
    (if (string-match "^\\([A-Z]:\\)" path)
        (replace-regexp-in-string
         "\\\\"
         "/"
         (replace-match (downcase (match-string 1 path)) t t path))
      path)))

(defun shell-command* (command &rest args)
  "Return a cons cell (code . output) after execute COMMAND in
 inferior shell.\n
See \\=`shell-command\\=' and \\=`shell-command-to-string\\=' for
details. If you want to set the environment temporarily that
\\=`shell-command*\\=' run in:
 (let ((process-environment (cons \"XXX=zzz\" process-environment)))
   (shell-command* \"echo $XXX\"))\n
Optional argument ARGS for COMMAND."
  (declare (indent 1))
  (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
    (unwind-protect
        (with-current-buffer b
          (cons
           (let ((x (call-process
                     shell-file-name nil b nil
                     shell-command-switch
                     (mapconcat #'identity
                                (apply #'list command args)
                                " "))))
             (cond ((integerp x) x)
                   ((string-match "^.*\\([0-9]+\\).*$" x)
                    (match-string-no-properties 1 x))
                   (t -1)))
           (let ((s (buffer-substring-no-properties
                     (point-min) (point-max))))
             (if (string= "\n" s) nil s))))
      (and b (kill-buffer b)))))

(defmacro executable-find% (command &optional fn)
  "Return the path of COMMAND at compile time.\n
Call FN with the path if FN is non-nil."
  (declare (pure t))
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
  (declare (pure t))
  (if (= most-positive-fixnum (1- (expt 2 61))) 64
    (if (= most-positive-fixnum (1- (expt 2 29))) 32 0)))

(defmacro platform-arch ()
  "Return platform architecture."
  (declare (pure t))
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

(defun version-strncmp (v1 v2 &optional n)
  "Return 0 if V1 equals V2, -1 if V1 less than V2, otherwise 1.\n
If optional N is non-nil compare no more than N parts, default N is 4."
  (let ((l1 (length v1)) (l2 (length v2))
        (nv1 0) (nv2 0)
        (n (or (and (integerp n) (> n 0) n) 4))
        (i 0) (j1 0) (j2 0) (k1 0) (k2 0))
    (cond ((and (= l1 0) (= l2 0)) 0)
          ((and (= l1 0) (> l2 0)) -1)
          ((and (> l1 0) (= l2 0) 1))
          (t (catch 'br
               (while (< i n)
                 (setq nv1
                       (catch 'br1
                         (when (= j1 l1) (throw 'br1 0))
                         (while (< j1 l1)
                           (when (= ?. (aref v1 j1))
                             (throw 'br1
                                    (string-to-number
                                     (substring-no-properties
                                      v1 k1
                                      (prog1 j1
                                        (setq j1 (1+ j1) k1 j1))))))
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
                                      v2 k2
                                      (prog1 j2
                                        (setq j2 (1+ j2) k2 j2))))))
                           (setq j2 (1+ j2)))
                         (string-to-number
                          (substring-no-properties v2 k2 j2))))
                 (cond ((< nv1 nv2) (throw 'br -1))
                       ((> nv1 nv2) (throw 'br 1))
                       ((= i (- n 1)) (throw 'br 0))
                       ((and (= j1 l1) (= j2 l2)) (throw 'br 0))
                       (t (setq i (1+ i))))))))))

;; end of platform

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

(defmacro require% (feature &optional filename noerror)
  "Require feature at compile-time."
  `(eval-when-compile
     (unless (featurep ,feature)
       (require ,feature ,filename ,noerror))))

 ;; end of compatible macro

;;;
;; file
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

(defun path- (file)
  "Return the parent path of FILE."
  (when (stringp file)
    (inhibit-file-name-handler
      (file-name-directory (directory-file-name file)))))

(defun path-depth (path &optional separator)
  "Return the depth of PATH."
  (if (= 0 (length path))
      0
    (- (length (split-string* path (or separator "/") nil)) 1)))

(defun file-in-dirs-p (file dirs)
  "Return the matched dir if FILE in DIRS, otherwise nil."
  (when (and (stringp file) (consp dirs))
    (inhibit-file-name-handler
      (let ((case-fold-search (when-platform% 'windows-nt t))
            (d (file-name-directory file)))
        (catch 'br
          (dolist* (x dirs)
            (and (stringp x)
                 (eq 't (compare-strings
                         x 0 (length x) d 0 (length x)
                         case-fold-search))
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
       (and (stringp ,r)
            (split-string* ,r "[:@]" t "\\(^/[^ssh].*$\\|^/\\)")))))

(defun ssh-remote->user@host (remote)
  "Norm the REMOTE to {user|id}[@host] form."
  (let ((rid (ssh-remote->ids remote)))
    (when (consp rid)
      (concat (cadr rid)
              (and (car (cddr rid))
                   (concat "@" (car (cddr rid))))))))

;; end of file

;;;
;; key
;;;

(defmacro kbd% (keys)
  "Convert KEYS to the internal Emacs key representation."
  (declare (pure t))
  (let ((-k%1- (kbd keys)))
    `,-k%1-))

(defmacro if-key% (keymap key test then &rest else)
  "If TEST yield t for KEY in KEYMAP do then, else do ELSE..."
  (declare (indent 4) (pure t))
  (let ((-ik%-m1- keymap)
        (-ik%-k1- (kbd key))
        (-ik%-t1- test))
    `(if% (eq ,-ik%-t1- (lookup-key ,-ik%-m1- ,-ik%-k1-))
         ,then
       ,@else)))

(defmacro define-key% (keymap key def)
  "Define KEY to DEF in KEYMAP."
  (let ((-dk%-m1- keymap)
        (-dk%-k1- (kbd key))
        (-dk%-d1- def))
    `(unless% (eq ,-dk%-d1- (lookup-key ,-dk%-m1- ,-dk%-k1-))
       (define-key ,-dk%-m1- ,-dk%-k1- ,-dk%-d1-))))

;; end of key

;;;
;; interactive
;;;

(defmacro if-region-active (then &rest else)
  "If \\=`mark-active\\=' is non-nil, do THEN, else do ELSE..."
  (declare (indent 1) (pure t))
  `(if mark-active
       ,then
     (progn% ,@else)))

(defmacro unless-region-active (&rest then)
  "Unless \\=`mark-active\\=' is non-nil, do THEN."
  (declare (indent 0))
  `(if-region-active nil ,@then))

 ;; end of interactive

(provide 'fn)

;; end of fn.el
