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
;; feature
;;;

(defmacro if-feature% (feature then &rest else)
  "If has the FEAUTURE do THEN, otherwise do ELSE..."
  (declare (indent 2))
  (if-feature `,feature
      `,then
    `(progn% ,@else)))

(defmacro when-feature% (feature &rest body)
  "When FEATURE do BODY."
  (declare (indent 1))
  `(if-feature% ,feature
       (progn% ,@body)
     (comment ,@body)))

(defmacro unless-feature% (feature &rest body)
  "Unless FEATURE do BODY."
  (declare (indent 1))
  `(if-feature% ,feature
       (comment ,@body)
     ,@body))

;; end of feature

;;;
;; *-fn%: checking fn existing
;;;

(defmacro if-fn% (fn feature then &rest else)
  "If the FN of FEATURE is bounded yield non-nil, do THEN, else do ELSE..."
  (declare (indent 3))
  (if-fn `,fn `,feature
         `,then
    `(progn% ,@else)))

(defmacro when-fn% (fn feature &rest body)
  "When the FN of FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  `(if-fn% ,fn ,feature (progn% ,@body)))

(defmacro unless-fn% (fn feature &rest body)
  "Unless the FN FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  `(if-fn% ,fn ,feature nil ,@body))

(unless-fn% declare-function nil
  (defmacro declare-function (&rest _)))

;; end of *-fn% macro


;;;
;; *-var%: checking var existing
;;;

(defmacro if-var% (var feature then &rest else)
  "If the VAR of FEATURE is bounded yield non-nil, do THEN, else do ELSE..."
  (declare (indent 3))
  (if-var `,var `,feature
          `,then
    `(progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "When the VAR FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  `(if-var% ,var ,feature (progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Unless the VAR FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  `(if-var% ,var ,feature nil ,@body))

(defmacro setq% (x val &optional feature)
  "Set X of FEATURE to the VAL when X is bound."
  ;; (declare (debug t))
  `(when-var% ,x ,feature
     (setq ,x ,val)))

;; end of *-var% macro


;;;
;; *lexical*: checking lexical context
;;;

(defmacro if-lexical% (then &rest else)
  "If lexical binding is built-in do THEN, otherwise do ELSE..."
  (declare (indent 1))
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
  (declare (indent 1))
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
  (declare (indent 2))
  (if (eq system-type os)
      `,then
    `(progn% ,@else)))

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
  (declare (indent 2))
  (if (eq window-system window)
      `,then
    `(progn% ,@else)))

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
;; if/when/unless-interactive%: checking `non-interactive' context
;;;

(defmacro if-interactive% (then &rest body)
  "If not \\=`noninteractive\\=' do THEN, else do BODY."
  (declare (indent 1))
  (if (null noninteractive)
      `,then
    `(progn% ,@body)))

(defmacro when-interactive% (&rest body)
  "When not \\=`noninteractive\\=' do BODY."
  (declare (indent 0))
  `(if-interactive% (progn% ,@body)))

(defmacro unless-interactive% (&rest body)
  "Unless not \\=`noninteractive\\=' do BODY."
  (declare (indent 0))
  `(if-interactive% nil ,@body))

;; end of if/when/unless-interactive% macro

;;;
;; alias
;;;

(fset 'range #'number-sequence)

(unless-fn% char= nil
  (fset 'char= #'char-equal))

(unless-fn% characterp nil
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

(unless-fn% take nil
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
  (let ((n1 (gensym*)) (i1 (gensym*)))
    `(let ((,n1 ,newelt) (,i1 ,idx))
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
  "Threading call FN with NAME in JOIN mode."
  (if (fboundp 'make-thread)
      (if join
          `(thread-join (make-thread ,fn ,name))
        `(make-thread ,fn ,name))
    `(ignore* ,join ,name)
    `(funcall ,fn)))

(defmacro thread-yield* ()
  "Yield the CPU to another thread."
  (when (fboundp 'thread-yield)
    `(thread-yield)))

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
  `(string-trim> (string-trim< ,s ,lr) ,rr))

(defun string-match* (regexp string num &optional start)
  "Return the NUMth match for REGEXP in STRING from START.\n
See \\=`string-match\\=' and \\=`match-string\\='."
  (let ((b (and (stringp string) (string-match regexp string start)
                (match-beginning num))))
    (and b (substring-no-properties string b (match-end num)))))

(when-version% > 24.4
  (defun split-string4 (string &optional separators omit-nulls trim)
    (let ((ss (split-string string separators omit-nulls)))
      (cond ((or (null ss) (= (length ss) 0)) ss)
            ((and (> (length ss) 0) trim (> (length trim) 0))
             (let ((xs nil))
               (while ss
                 (let ((s (car ss)))
                   (setq xs (cons (if (and (stringp trim) (> (length trim) 0))
                                      (string-trim>< s trim trim)
                                    (string-trim>< s))
                                  xs)))
                 (setq ss (cdr ss)))
               (nreverse xs)))
            (t ss)))))

(defmacro split-string* (string &optional separators omit-nulls trim)
  "Split STRING into substrings bounded by match for SEPARATORS.\n
Optional argument OMIT-NULLS omit null strings.
Optional argument TRIM regexp used to trim."
  (if-version%
      > 24.4
      `(split-string4 ,string ,separators ,omit-nulls ,trim)
    `(split-string ,string ,separators ,omit-nulls ,trim)))

;; end of string

;;;
;; IO
;;;

(defmacro get-buffer-create* (buffer-or-name &optional inhibit-buffer-hooks)
  "See \\=`get-buffer-create\\='."
  (if-version%
      <= 28
      `(get-buffer-create ,buffer-or-name ,inhibit-buffer-hooks)
    `(if ,inhibit-buffer-hooks
         (let ((kill-buffer-hook nil)
               (kill-buffer-query-functions nil)
               (buffer-list-update-hook nil))
           (get-buffer-create ,buffer-or-name))
       (get-buffer-create ,buffer-or-name))))

(defmacro insert-file-contents-literally*
    (filename &optional visit beg end replace)
  "See \\=`insert-file-contents-literally\\='."
  `(let ((format-alist nil)
         (after-insert-file-functions nil)
         (file-coding-system-alist nil)
         (coding-system-for-read 'no-conversion)
         (coding-system-for-write 'no-conversion))
     (insert-file-contents ,filename ,visit ,beg ,end ,replace)))

(defmacro write-region*
    (start end filename &optional append visit lockname mustbenew)
  "See \\=`write-region\\='."
  `(let ((format-alist nil)
         (coding-system-for-write 'no-conversion)
         (write-region-inhibit-fsync t)
         (write-region-annotate-functions nil))
     (write-region ,start ,end ,filename ,append ,visit ,lockname ,mustbenew)))

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
  (let* ((sep (strrchr path ?/))
         (from (if sep (1+ sep) 0))
         (to (strrchr path ?.)))
    (substring-no-properties path from (and to (> to from) to))))

(unless-fn% directory-name-p nil
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
  "Return a cons cell (code . output) after execute COMMAND
 in inferior shell.\n
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
          (let* ((c (if (consp args)
                        (mapconcat #'identity (nconc (list command) args) " ")
                      command))
                 (x (call-process
                     shell-file-name nil b nil
                     shell-command-switch
                     c)))
            (cons x (buffer-substring-no-properties 1 (point-max)))))
      (and b (kill-buffer b)))))

(defun executable-find* (command &optional fn)
  "Return the path of COMMAND.\n
Call FN with the path if FN is non-nil."
  (let ((rc (shell-command* (if-platform% windows-nt
                                "where"
                              "command -v")
              command)))
    (when (zerop (car rc))
      (let ((ss (string-trim> (cdr rc))))
        (cond (fn (funcall fn (shell-quote-argument ss)))
              (t (if-platform% windows-nt
                     (posix-path ss)
                   ss)))))))

(defmacro executable-find% (command &optional fn)
  "Return from \\=excutable-find*\\= at compile time."
  (executable-find* command fn))

(defmacro emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (cond ((= most-positive-fixnum (1- (expt 2 61))) 64)
        ((= most-positive-fixnum (1- (expt 2 29))) 32)
        (t 16)))

(defmacro platform-arch ()
  "Return platform architecture."
  (let* ((m64 "\\([xX]86_64\\|[aA][mM][dD]64\\|aarch64\\|arm64\\)")
         (x64 (string-match* m64 system-configuration 1)))
    (if x64 x64
      (if-platform% windows-nt
          (let* ((cpu "PROCESSOR_ARCHITECTURE")
                 (c64 (string-match* m64 (getenv-internal cpu) 1)))
            (if c64 c64 (getenv-internal cpu)))
        (let ((m (shell-command* "uname" "-m")))
          (cond ((zerop (car m)) (string-trim> (cdr m)))
                (t nil)))))))

;; end of platform

;;;
;; compatible macro
;;;

(unless-fn% user-error nil
  (defun user-error (format &rest args)
    "Signal a pilot error."
    (signal 'user-error
            (list (apply #'format-message format args)))))

(unless-fn% with-eval-after-load nil
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded."
    (declare (indent 1))
    `(eval-after-load ,file (lambda () ,@body))))

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
  (when (and (stringp root) (> (length root) 0))
    (let ((ss root) (pc ?/) (ps "/"))
      (unless (char-equal (aref ss (1- (length ss))) pc)
        (setq ss (concat ss ps)))
      (while path
        (let* ((s (car path)) (len (length s)))
          (when (> len 0)
            (let ((i 0) (j (1- len)))
              (while (and (< i len) (char-equal (aref s i) pc))
                (setq i (1+ i)))
              (while (and (> j i) (char-equal (aref s j) pc))
                (setq j (1- j)))
              (setq ss (concat ss (substring-no-properties s i (1+ j))))
              (unless (char-equal (aref ss (1- (length ss))) pc)
                (setq ss (concat ss ps))))))
        (setq path (cdr path)))
      (unless (char-equal (aref ss (1- (length ss))) pc)
        (setq ss (concat ss ps)))
      ss)))

(defun path- (file)
  "Return the parent path of FILE."
  (when (stringp file)
    (inhibit-file-name-handler
      (file-name-directory (directory-file-name file)))))

(defun path-depth (path &optional separator)
  "Return the depth of PATH."
  (cond ((= 0 (length path)) 0)
        (t (- (length (split-string* path (or separator "/") nil)) 1))))

;; end of file

;;;
;; key
;;;

(defmacro kbd% (key)
  "Convert KEY to the internal Emacs key representation."
  (kbd key))

(defmacro if-key% (keymap key def then &rest else)
  "If DEF for KEY in KEYMAP do THEN, else do ELSE..."
  (declare (indent 4))
  `(if% (eq ,def (lookup-key ,keymap ,key))
       ,then
     (progn% ,@else)))

(defmacro define-key% (keymap key def)
  "Define KEY to DEF in KEYMAP."
  ;; (declare (debug t))
  (let ((-dkk3- (cond ((consp key) (funcall `(lambda () ,key)))
                      (t key))))
    (unless (eq def (lookup-key (symbol-value keymap) -dkk3-))
      `(define-key ,keymap ,-dkk3- ,def))))

(defmacro define-global-key% (key def)
  "Define KEY to DEF in \\=`global-map\\='."
  ;; (declare (debug t))
  (let ((-dkk3- (cond ((consp key) (funcall `(lambda () ,key)))
                      (t key))))
    (unless (eq def (lookup-key global-map -dkk3-))
      `(define-key global-map ,-dkk3- ,def))))

;; end of key

;;;
;; interactive
;;;

(defmacro if-region-active (then &rest else)
  "If \\=`mark-active\\=' is non-nil, do THEN, else do ELSE..."
  (declare (indent 1))
  `(if mark-active
       ,then
     (progn% ,@else)))

(defmacro unless-region-active (&rest body)
  "Unless \\=`mark-active\\=' is non-nil, do BODY."
  (declare (indent 0))
  `(if-region-active nil ,@body))

 ;; end of interactive

(provide 'fn)

;; end of fn.el
