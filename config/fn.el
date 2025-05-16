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
;; *-home%
;;;

(defmacro emacs-home% (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\=' at compile-time."
  (emacs-home* file))

(defmacro v-home% (&optional file)
  "Return versioned path of FILE under \\=`v-home\\=' at compile-time."
  (v-home* file))

(defmacro v-home%> (file)
  "Return the \\=`v-home\\=' FILE with the extension of compiled file."
  (concat (v-home* file) +comp-file-extension+))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home\\=' at compile-time."
  (make-v-home* file))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (funcall `(lambda () (compile-unit* ,file ,only-compile))))

;; end of *-home%

;;;
;; feature
;;;

(defmacro if-feature% (feature then &rest else)
  "If has the FEAUTURE do THEN, otherwise do ELSE..."
  (declare (indent 2))
  (if (feature? feature)
      `,then
    `(progn% ,@else)))

(defmacro when-feature% (feature &rest body)
  "When FEATURE do BODY."
  (declare (indent 1))
  (when (feature? feature)
    `(progn% ,@body)))

(defmacro unless-feature% (feature &rest body)
  "Unless FEATURE do BODY."
  (declare (indent 1))
  (unless (feature? feature)
    `(progn% ,@body)))

;; end of feature

;;;
;; *-fn%: checking fn existing
;;;

(defmacro if-fn% (fn feature then &rest else)
  "If the FN of FEATURE is bounded yield non-nil, do THEN, else do ELSE..."
  (declare (indent 3))
  (if (fn? fn feature)
      `,then
    `(progn% ,@else)))

(defmacro when-fn% (fn feature &rest body)
  "When the FN of FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  (when (fn? fn feature)
    `(progn% ,@body)))

(defmacro unless-fn% (fn feature &rest body)
  "Unless the FN FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  (unless (fn? fn feature)
    `(progn% ,@body)))

(unless-fn% declare-function nil
  (defmacro declare-function (&rest _)))

;; end of *-fn% macro


;;;
;; *-var%: checking var existing
;;;

(defmacro if-var% (var feature then &rest else)
  "If the VAR of FEATURE is bounded yield non-nil, do THEN, else do ELSE..."
  (declare (indent 3))
  (if (var? var feature)
      `,then
    `(progn% ,@else)))

(defmacro when-var% (var feature &rest body)
  "When the VAR FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  (when (var? var feature)
    `(progn% ,@body)))

(defmacro unless-var% (var feature &rest body)
  "Unless the VAR FEATURE is bounded yield non-nil, do BODY."
  (declare (indent 2))
  (unless (var? var feature)
    `(progn% ,@body)))

(defmacro setq% (x val &optional feature)
  "Set X of FEATURE to the VAL when X is bound."
  ;; (declare (debug t))
  (when (var? x feature)
    `(setq ,x ,val)))

;; end of *-var% macro


;;;
;; *lexical*: checking lexical context
;;;

(defmacro if-lexical% (then &rest else)
  "If lexical binding is built-in do THEN, otherwise do ELSE..."
  (declare (indent 1))
  (if (var? lexical-binding)
      `,then
    `(progn% ,@else)))

(defmacro when-lexical% (&rest body)
  "When lexical binding is built-in do BODY."
  (declare (indent 0))
  (if-lexical%
      `(progn% ,@body)
    `(comment ,@body)))

(defmacro unless-lexical% (&rest body)
  "Unless lexical binding is built-in do BODY."
  (declare (indent 0))
  (if-lexical%
      `(comment ,@body)
    `(progn% ,@body)))

(defmacro ignore* (&rest vars)
  "Return nil, list VARS at compile time if in lexical context."
  (declare (indent 0))
  (when-lexical% `(prog1 nil ,@vars)))

(defun true (&rest x)
  "Return true value and ignore X."
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
  (if +display-graphic+
      `,then
    `(progn% ,@else)))

(defmacro when-graphic% (&rest body)
  "When \\=`display-graphic-p\\=' yield non-nil, do BODY."
  (declare (indent 0))
  (if-graphic%
      `(progn% ,@body)
    `(comment ,@body)))

(defmacro unless-graphic% (&rest body)
  "Unless \\=`display-graphic-p\\=' yield nil, do BODY."
  (declare (indent 0))
  (if-graphic%
      `(comment ,@body)
    `(progn% ,@body)))

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
  (when (eq system-type os)
    `(progn% ,@body)))

(defmacro unless-platform% (os &rest body)
  "Unless OS eq \\=`system-type\\=' yield non-nil do BODY."
  (declare (indent 1))
  (unless (eq system-type os)
    `(progn% ,@body)))

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
  (when (eq window-system window)
    `(progn% ,@body)))

(defmacro unless-window% (window &rest body)
  "Unless WINDOW eq \\=`initial-window-system\\=' yield non-nil, do BODY."
  (declare (indent 1))
  (unless (eq window-system window)
    `(progn% ,@body)))

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
  (if-interactive%
      `(progn% ,@body)
    `(comment ,@body)))

(defmacro unless-interactive% (&rest body)
  "Unless not \\=`noninteractive\\=' do BODY."
  (declare (indent 0))
  (if-interactive%
      `(comment ,@body)
    `(progn% ,@body)))

;; end of if/when/unless-interactive% macro

;;;
;; alias
;;;

(unless-fn% characterp nil
  (defmacro characterp (object)
    `(char-valid-p ,object)))

;; end of alias

;;;
;; sequence
;;;

(defmacro append! (newelt seq &optional uniquely)
  "Append NEWELT to the end of SEQ.\n
If optional UNIQUELY is non-nil then append uniquely."
  (let ((n1 (gensym*)) (s1 (gensym*)))
    `(let ((,n1 ,newelt) (,s1 ,seq))
       (setq ,seq (nconc ,(if uniquely
                              `(,uniquely ,n1 ,s1)
                            `,s1)
                         (list ,n1))))))

(defmacro fluid-let (binding &rest body)
  "Execute BODY and restore the BINDING after return."
  (declare (indent 1))
  (let ((old (gensym*))
        (var (car binding))
        (new (gensym*)))
    `(let ((,old ,(car binding))
           (,new ,(cadr binding)))
       (unwind-protect
           (progn
             (setq ,var ,new)
             ,@body)
         (setq ,var ,old)))))

(defmacro push! (newelt seq &optional uniquely)
  "Push NEWELT to the head of SEQ.\n
If optional UNIQUELY is non-nil then push uniquely."
  (let ((n1 (gensym*)))
    `(let ((,n1 ,newelt))
       (setq ,seq ,(if uniquely
                       `(cons ,n1 (,uniquely ,n1 ,seq))
                     `(cons ,n1 ,seq))))))

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
    (catch :br
      (while (< i l)
        (and (char-equal chr (aref str i)) (throw :br i))
        (setq i (1+ i))))))

(defun strrchr (str chr)
  "Return the index of the located CHR of STR from right side."
  (let* ((l (length str)) (i (1- l)))
    (catch :br
      (while (>= i 0)
        (and (char-equal chr (aref str i)) (throw :br i))
        (setq i (1- i))))))

(defun strawk (str pattern)
  "Return the processed STR via awk\\='s PATTERN processing."
  (while (car pattern)
    (let ((s1 (car (car pattern))) (s2 (cdr (car pattern)))
          (start 0) (r1 nil))
      (while (string-match s1 str start)
        (setq r1 (cond ((stringp s2) s2)
                       ((integerp s2)
                        (char-to-string
                         (string-to-number
                          (substring-no-properties
                           str (match-beginning 1) (match-end 1))
                          s2)))
                       ((functionp s2) (funcall s2 str))
                       (t ""))
              str (replace-match r1 nil nil str)
              start (+ (match-beginning 0) (length r1)))))
    (setq pattern (cdr pattern)))
  str)

(defun string-trim> (s &optional rr)
  "Remove tailing whitespaces or matching of RR at the end of S."
  (and (stringp s)
       (let* ((r1 (concat "\\(?:" (or rr "[ \t\n\r]+") "\\)\\'"))
              (i (string-match r1 s 0)))
         (if i (substring-no-properties s 0 i) s))))

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

(defun write-file* (object file)
  "Write OBJECT to FILE.\n
Return the name of FILE when successed otherwise nil."
  (inhibit-file-name-handler
    (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
      (unwind-protect
          (with-current-buffer b
            (if (stringp object)
                (princ object b)
              (prin1 object b))
            (let ((format-alist nil)
                  (coding-system-for-write 'no-conversion)
                  (write-region-inhibit-fsync t)
                  (write-region-annotate-functions nil))
              (write-region (point-min) (point-max) file nil :slient))
            file)
        (and b (kill-buffer b))))))

(defun read-file* (file &optional sexp?)
  "Read the string or SEXP from FILE."
  (inhibit-file-name-handler
    (when (and (stringp file) (file-exists-p file))
      (let ((b (get-buffer-create* (symbol-name (gensym*)) t)))
        (unwind-protect
            (with-current-buffer b
              (let ((format-alist nil)
                    (after-insert-file-functions nil)
                    (file-coding-system-alist nil)
                    (coding-system-for-read 'no-conversion)
                    (coding-system-for-write 'no-conversion))
                (insert-file-contents file))
              (let ((s (buffer-substring-no-properties
                        (point-min) (point-max))))
                (if sexp?
                    (and (> (length s) 0) (read s))
                  s)))
          (and b (kill-buffer b)))))))

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
      (and (> len 0) (char-equal ?/ (aref name (1- len)))))))

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
  "Return a cons cell (code . output) after execute COMMAND.\n
Optional ARGS for COMMAND.
If you want to set the environment temporarily such that:
 (let ((process-environment (cons \"XXX=zzz\" process-environment))
       (shell-file-name \"sh\")
       (shell-command-switch \"-c\")
   (shell-command* \"echo $XXX\"))\n
See \\=`shell-command\\=' and \\=`shell-command-to-string\\='."
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
Call FN with the path of COMMAND if FN is non-nil."
  (inhibit-file-name-handler
    (let* ((path exec-path)
           (X_OK 1)
           (rc (locate-file-internal command path nil X_OK)))
      (when rc
        (cond (fn (funcall fn (shell-quote-argument rc)))
              (t (if-platform% windows-nt
                     (posix-path rc)
                   rc)))))))

(defmacro executable-find% (command &optional fn)
  "Return from \\=excutable-find*\\= at compile time."
  (executable-find* command fn))

(defun emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (cond ((= most-positive-fixnum (1- (expt 2 61))) 64)
        ((= most-positive-fixnum (1- (expt 2 29))) 32)
        (t 16)))

(defmacro emacs-arch% ()
  (emacs-arch))

;; end of platform

;;;
;; compatible macro
;;;

(defmacro defadvice* (symbol fn1 fn2)
  "Define the FN2 advice for FN1."
  (declare (indent 1))
  `(progn
     (fset ,symbol (symbol-function ,fn1))
     (fset ,fn1 ,fn2)))

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

 ;; end of compatible macro

;;;
;; file
;;;

(defun path! (file)
  (inhibit-file-name-handler
    (if (file-exists-p file)
        file
      (mkdir* file))))

(defun path+ (root &rest path)
  "Append a list of PATH to ROOT."
  (declare (indent 1))
  (cond ((null path) root)
        ((= 0 (length root)) (apply #'path+ (car path) (cdr path)))
        ((= 0 (length (car path))) (apply #'path+ root (cdr path)))
        ((char-equal ?/ (aref root (1- (length root))))
         (apply #'path+
                (substring-no-properties root 0 (1- (length root)))
                path))
        ((char-equal ?/ (aref (car path) 0))
         (apply #'path+ (concat root (car path)) (cdr path)))
        (t (apply #'path+ (concat root "/" (car path)) (cdr path)))))

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

(defun key? (keymap key def)
  "Return KEY if DEF for KEY in KEYMAP, otherwise nil"
  (if (eq def (lookup-key keymap key))
      (cons key def)
    (cons key nil)))

(defmacro kbd% (key)
  "Convert KEY to the internal Emacs key representation."
  (kbd key))

(defmacro if-key% (keymap key def then &rest else)
  "If DEF for KEY in KEYMAP do THEN, else do ELSE..."
  (declare (indent 4))
  (let ((-ik1- (funcall `(lambda () (key? ,keymap ,key ,def)))))
    (if (cdr -ik1-)
        `,then
      `(progn% ,@else))))

(defmacro define-key% (keymap key def)
  "Define KEY to DEF in KEYMAP."
  ;; (declare (debug t))
  (let ((-dk1- (funcall `(lambda () (key? ,keymap ,key ,def)))))
    (unless (cdr -dk1-)
      `(define-key ,keymap ,(car -dk1-) ,def))))

(defmacro define-global-key% (key def)
  "Define KEY to DEF in \\=`global-map\\='."
  ;; (declare (debug t))
  (let ((-dgk1- (funcall `(lambda () (key? global-map ,key ,def)))))
    (unless (cdr -dgk1-)
      `(define-key global-map ,(car -dgk1-) ,def))))

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
