;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; fns.el
;;;;


(defalias 'range #'number-sequence)

(unless-fn% 'char= nil
  (defalias 'char= #'char-equal))

(unless-fn% 'characterp nil
  (defalias 'characterp #'char-valid-p))


(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))


(defun take (n seq)
  "Returns a sequence of the first N items in SEQ.

Or all items if SEQ has fewer items than N."
  (let ((acc nil) (n1 n) (s1 seq))
    (while (and (> n1 0) s1)
      (setq acc (cons (car s1) acc)
            n1 (1- n1) s1 (cdr s1)))
    (nreverse acc)))


(defun drop-while (pred seq)
  "Returns a sequence of successive items from SEQ after the item 
for which (PRED item) returns t."
  (let ((s seq) (w nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s (cdr s))))
    (cdr s)))


(defun take-while (pred seq)
  "Returns a sequence of successive items from SEQ before the
item for which (PRED item) returns t."
  (let ((s seq) (w nil) (s1 nil))
    (while (and (not w) (car s))
      (if (funcall pred (car s))
          (setq w t)
        (setq s1 (cons (car s) s1)
              s (cdr s))))
    (nreverse s1)))


;;;;
;; common lisp macro
;;;;

;; Load cl-lib/cl at runtime
(if-version% <= 24
             (when-version% > 26
               (require 'cl-lib))
  (with-no-warnings
    (require 'cl)))


(defmacro lexical-let% (varlist &rest body)
  "Like `let', but lexically scoped."
  (declare (indent 1) (debug let))
  `(if-lexical%
       (if-version% < 27
                    (let ,varlist ,@body)
         (let ((lexical-binding t))
           (let ,varlist ,@body)))
     (when-fn% 'lexical-let 'cl
       (lexical-let ,varlist ,@body))))

(defmacro lexical-let*% (varlist &rest body)
  "Like `let*', but lexically scoped."
  (declare (indent 1) (debug let))
  `(if-lexical%
       `(let ((lexical-binding t))
          (let ,varlist ,@body))
     (when-fn% 'lexical-let* 'cl
       (lexical-let* ,varlist ,@body))))


(defmacro assoc** (key list &optional testfn)
  "Return non-nil if KEY is equal to the `car' of an element of LIST.

The value is actually the first element of LIST whose car equals KEY.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (if-version%
      <= 26.1
      `(assoc ,key ,list ,testfn)
    (if-fn% 'cl-assoc 'cl-lib
            (progn%
             (declare-function cl-assoc "cl-seq.elc"
                               (item seq &rest keys)
                               t)
             `(cl-assoc ,key ,list :test (or ,testfn #'equal)))
      (when-fn% 'assoc* 'cl
        `(with-no-warnings
           (assoc* ,key ,list :test (or ,testfn #'equal)))))))


;; Unify `cl-mapcar' and `mapcar*'
(defmacro mapcar** (fn seq &rest seqs)
  "Apply FUNCTION to each element of SEQ, and make a list of the results.
If there are several SEQs, FUNCTION is called with that many arguments,
and mapping stops as soon as the shortest list runs out.  With just one
SEQ, this is like `mapcar'.  With several, it is like the Common Lisp
`mapcar' function extended to arbitrary sequence types.
\n(fn FUNCTION SEQ...)"
  (if-fn% 'cl-mapcar 'cl-lib
          (if-version% <= 25
                       `(cl-mapcar ,fn ,seq ,@seqs)
            (declare-function cl-mapcar "cl-lib.elc"
                              (fn x &rest rest)
                              t)
            `(cl-mapcar ,fn ,seq ,@seqs))
    `(when-fn% 'mapcar* 'cl
       (with-no-warnings
         (mapcar* ,fn ,seq ,@seqs)))))


(defmacro remove-if* (pred seq &rest keys)
  "Remove all items satisfying PRED in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :key :count :start :end :from-end
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)"  
  (if-fn% 'cl-remove-if 'cl-lib
          (if-version% <= 25
                       `(cl-remove-if ,pred ,seq ,@keys)
            (declare-function cl-remove-if "cl-seq.elc"
                              (pred seq &rest keys)
                              t)
            `(cl-remove-if ,pred ,seq ,@keys))
    `(with-no-warnins
      (remove-if ,pred ,seq ,@keys))))


(defmacro member-if* (pred list &rest keys)
  "Find the first item satisfying PRED in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)"
  (if-fn% 'cl-member-if 'cl-lib
          (if-version% <= 25
                       `(cl-member-if ,pred ,list ,@keys)
            (declare-function cl-member-if (pred seq &rest keys)
                              t)
            `(cl-member-if ,pred ,list ,@keys))
    `(with-no-warnings
       (member-if ,pred ,list ,@keys))))


(defmacro every* (pred &rest seq)
  "Return t if PRED is true of every element of SEQ or SEQs."
  (declare (indent 1))
  (if-fn% 'cl-every 'cl-lib
          (if-version% <= 25
                       `(cl-every ,pred ,@seq)
            (declare-function cl-every "cl-extra.elc"
                              (pred seq &rest rest)
                              t)
            `(cl-every ,pred ,@seq))
    (when-fn% 'every 'cl
      `(with-no-warnings
         (every ,pred ,@seq)))))


(defmacro some* (pred &rest seq)
  "Return t if PRED is true of any element of SEQ or SEQs."
  (declare (indent 1))
  (if-fn% 'cl-some 'cl-lib
          (if-version% <= 25
                       `(cl-some ,pred ,@seq)
            (declare-function cl-some "cl-extra.elc"
                              (pred seq &rest rest)
                              t)
            `(cl-some ,pred ,@seq))
    (when-fn% 'some 'cl
      `(with-no-warnings
         (some ,pred ,@seq)))))

(defmacro loop* (&rest loop-args)
  "The Common Lisp `loop' macro."
  (if-fn% 'cl-loop 'cl-lib
          `(cl-loop ,@loop-args)
    (when-fn% 'loop 'cl
      `(with-no-warnings
         (loop ,@loop-args)))))

 ;; end of common lisp macro


;;;;
;; byte-compiler macro
;;;;

(defmacro ignore* (&rest vars)
  "Return nil, list VARS at compile time if in lexical context."
  (declare (indent 0))
  (when-lexical%
    `(when% lexical-binding
       (progn% ,@vars nil))))


(defmacro defmacro-if-feature% (feature &optional docstring)
  "Define if-FEATURE% compile-time macro."
  (let ((name (intern (format "if-feature-%s%%" feature)))
        (ds1 (format "If has `%s' feauture do THEN, otherwise do BODY."
                     feature)))
    `(defmacro ,name (then &rest body)
       ,(or docstring ds1)
       (declare (indent 1))
       (if% (require ',feature nil t)
           `(progn% (comment ,@body)
                    ,then)
         `(progn% (comment ,then)
                  ,@body)))))


(defmacro make-thread* (sexp &optional join name)
  "Make SEXP as threading call."
  `(if-fn% 'make-thread nil
           (let ((thread (make-thread (lambda () ,sexp) ,name)))
             (if% ,join
                 (thread-join thread)
               thread))
     (ignore* ,join ,name)
     (funcall (lambda () ,sexp))))


(defmacro dolist* (spec &rest body)
  "Loop over a list.
Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

 (dolist* (VAR LIST [RESULT]) BODY...)"
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


 ;; end of byte-compiler macro

;;;;
;; Strings
;;;;

(defsubst string-trim> (s &optional rr)
  "Remove whitespaces or the matching of RR at the end of S."
  (when (stringp s)
    (let ((r (if rr (concat rr "\\'") "[ \t\n\r]+\\'" )))
      (if (string-match r s)
          (replace-match "" t t s)
        s))))


(defsubst string-trim< (s &optional lr)
  "Remove leading whitespace or the matching of LR from S."
  (when (stringp s)
    (let ((r (if lr (concat "\\`" lr) "\\`[ \t\n\r]+")))
      (if (string-match r s)
          (replace-match "" t t s)
        s))))


(defsubst string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespace or the matching of LR/RR from S."
  (let ((s1 (string-trim> s rr)))
    (string-trim< s1 lr)))


(defsubst match-string* (regexp string num &optional start)
  "Return string of text match for REGEXP in STRING.

Return nil if NUMth pair didn’t match, or there were less than NUM pairs.
NUM specifies which parenthesized expression in the REGEXP.
If START is non-nil, start search at that index in STRING.

See `string-match' and `match-string'."
  (when (and (stringp string)
             (string-match regexp string start)
             (match-beginning num))
    (substring string (match-beginning num) (match-end num))))


(defmacro split-string* (string &optional separators omit-nulls trim)
  "Split STRING into substrings bounded by matches for SEPARATORS, 
like `split-string' in Emacs 24.4+"
  (if-version%
      <= 24.4
      `(split-string ,string ,separators ,omit-nulls ,trim)
    `(if ,trim
         (delete ""
                 (mapcar (lambda (s)
                           (if (and (stringp ,trim) (> (length ,trim) 0))
                               (string-trim>< s ,trim ,trim)
                             (string-trim>< s)))
                         (split-string ,string ,separators ,omit-nulls)))
       (split-string ,string ,separators ,omit-nulls))))


 ;; end of Strings


(defmacro shell-command* (command &rest args)
  "Return a cons cell (code . output) after execute COMMAND in inferior shell.

See `shell-command' and `shell-command-to-string' for details.

If you want to set the environment temporarily that
shell-command* run in:
 (let ((process-environment (cons \"GREP_OPTIONS=' --color=always'\" 
                                   process-environment)))
   (shell-command* \"echo 'a' | grep 'a'\"))"
  (declare (indent 1))
  `(with-temp-buffer
     (cons (call-process shell-file-name nil (current-buffer) nil
                         shell-command-switch
                         (mapconcat #'identity
                                    (cons ,command (list ,@args)) " "))
           (let ((s (buffer-string)))
             (if (string= "\n" s) nil s)))))


(defmacro executable-find% (command &optional prefer)
  "Search for COMMAND in %PATH% or $PATH and return the absolute
file name at compile-time when PREFER is non-nil, otherwise same
as `executable-find'.

Return nil if no COMMAND found or no PREFER command found.
Return the first matched one, if multiple COMMANDs had been found
or the one that `funcall' PREFER returns t.
"
  (if prefer
      (let ((cmd (shell-command* (if-platform% 'windows-nt
                                     "where"
                                   "command -v")
                   (funcall `(lambda () ,command)))))
        (when (zerop (car cmd))
          (let* ((ss (cdr cmd))
                 (path (split-string* ss "\n" t))
                 (p (cond
                     ((and (consp path) (functionp prefer))
                      (catch 'prefer
                        (dolist* (x path)
                          (when (funcall prefer
                                         (shell-quote-argument
                                          (if-platform% 'windows-nt
                                              (windows-nt-posix-path x)
                                            x)))
                            (throw 'prefer x)))
                        nil))
                     ((consp path) (car path))
                     (t path))))
            `,(when p (if-platform% 'windows-nt
                          (windows-nt-posix-path p)
                        p)))))
    (let ((path (executable-find (funcall `(lambda () ,command)))))
      (ignore* prefer)
      `,path)))



;; end of file
