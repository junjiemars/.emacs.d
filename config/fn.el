;;; fn.el --- primitive functions -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;;; Commentary:
;;
;;; Code:

(defalias 'range #'number-sequence)

(unless-fn% 'char= nil
  (fset 'char= #'char-equal))

(unless-fn% 'characterp nil
  (fset 'characterp #'char-valid-p))


(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))


(defun take (n seq)
  "Return a sequence of the first N items in SEQ."
  (let ((s))
    (while (and (> n 0) seq)
      (setq s (cons (car seq) s)
            n (1- n)
            seq (cdr seq)))
    (nreverse s)))


(defun take-while (pred seq)
  "Return a sequence of items from SEQ just take while PRED is t."
  (let ((s))
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


(defmacro push! (newelt seq &optional append uniquely)
  "Push NEWELT to the ahead or back of SEQ.

If optional APPEND is non-nil then push back else push ahead.
If optional UNIQUELY is non-nil then push uniquely."
  (let ((n1 (gensym))
        (s1 (gensym)))
    `(let* ((,n1 ,newelt)
            (,s1 (if ,uniquely (delete ,n1 ,seq) ,seq)))
       (setq ,seq (if ,append
                      (append ,s1 (list ,n1))
                    (cons ,n1 ,s1))))))




;;;;
;; common lisp macro
;;;;

;; Load cl-lib/cl at runtime
(if-version% <= 24
             (when-version% > 26
               (require 'cl-lib))
  (with-no-warnings
    (require 'cl)))


(defmacro assoc** (key list &optional testfn)
  "Return non-nil if KEY is equal to the `car' of an element of LIST.

The value is actually the first element of LIST whose car equals KEY.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (if-version%
      <= 26.1
      `(assoc ,key ,list ,testfn)
    (if-fn% 'cl-assoc 'cl-lib
            (progn%
             (declare-function cl-assoc "cl-seq"
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
\n(FN FUNCTION SEQ...)"
  (if-fn% 'cl-mapcar 'cl-lib
          (if-version% <= 25
                       `(cl-mapcar ,fn ,seq ,@seqs)
            (declare-function cl-mapcar "cl-lib"
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
\n(fn PREDICATE SEQ [KEYWORD VALUE]...)
Optional argument KEYS :key :count :start :end :from-end."
  (if-fn% 'cl-remove-if 'cl-lib
          (if-version% <= 25
                       `(cl-remove-if ,pred ,seq ,@keys)
            (declare-function cl-remove-if "cl-seq"
                              (pred seq &rest keys)
                              t)
            `(cl-remove-if ,pred ,seq ,@keys))
    `(with-no-warnins
      (remove-if ,pred ,seq ,@keys))))


(defmacro member-if* (pred list &rest keys)
  "Find the first item satisfying PRED in LIST.
Return the sublist of LIST whose car matches.
\nKeywords supported:  :key
\n(fn PREDICATE LIST [KEYWORD VALUE]...)
Optional argument KEYS :key."
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
            (declare-function cl-every "cl-extra"
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
            (declare-function cl-some "cl-extra"
                              (pred seq &rest rest)
                              t)
            `(cl-some ,pred ,@seq))
    (when-fn% 'some 'cl
      `(with-no-warnings
         (some ,pred ,@seq)))))

(defmacro loop* (&rest clause)
  "The Common Lisp `loop' macro.
Optional argument CLAUSE such as for clause, iteration clause,
accumulate clause and Miscellaneous clause."
  (if-fn% 'cl-loop 'cl-lib
          `(cl-loop ,@clause)
    (when-fn% 'loop 'cl
      `(with-no-warnings
         (loop ,@clause)))))


(defmacro time (&rest form)
  "Execute FORM and print timing information on *message*."
  (declare (indent 0))
  (let ((b (gensym)))
    `(let ((,b (current-time)))
       (prog1
           (progn ,@form)
         (message "%.6fs"
                  (float-time (time-subtract (current-time) ,b)))))))


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
  "Define if-FEATURE% compile-time macro.

Argument FEATURE that defining.
Optional argument DOCSTRING about FEATURE."
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


(defmacro make-thread* (fn &optional join name)
  "Threading call FN with NAME or in JOIN mode."
  `(if-fn% 'make-thread nil
           (let ((thread (make-thread ,fn ,name)))
             (if% ,join
                 (thread-join thread)
               thread))
     (ignore* ,join ,name)
     (funcall ,fn)))


(defmacro fluid-let (binding &rest body)
  "Execute BODY and restore the BINDING after return."
  (declare (indent 1))
  (let ((old (gensym))
        (var (car binding))
        (new (gensym)))
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

(defmacro string-trim> (s &optional rr)
  "Remove tailing whitespaces or matching of RR at the end of S."
  (let ((s1 (gensym))
        (r1 (gensym)))
    `(let ((,s1 ,s))
       (when (stringp ,s1)
         (let ((,r1 (if ,rr (concat ,rr "\\'") "[ \t\n\r]+\\'" )))
           (if (string-match ,r1 ,s1)
               (replace-match "" t t ,s1)
             ,s1))))))


(defmacro string-trim< (s &optional lr)
  "Remove leading whitespaces or matching of LR from S."
  (let ((s1 (gensym))
        (l1 (gensym))
        (r1 (gensym)))
    `(let ((,s1 ,s))
       (when (stringp ,s1)
         (let* ((,l1 ,lr)
                (,r1 (if ,l1 (concat "\\`" ,l1) "\\`[ \t\n\r]+")))
           (if (string-match ,r1 ,s1)
               (replace-match "" t t ,s1)
             ,s1))))))


(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespaces or matching of LR/RR from S."
  (let ((s1 (gensym))
        (r1 (gensym)))
    `(let* ((,r1 ,rr)
            (,s1 (string-trim> ,s ,r1)))
       (string-trim< ,s1 ,lr))))


(defmacro match-string* (regexp string num &optional start)
  "Return string of text match for REGEXP in STRING.

Return nil if NUMth pair didn’t match, or there were less than NUM pairs.
NUM specifies which parenthesized expression in the REGEXP.
If START is non-nil, start search at that index in STRING.

See `string-match' and `match-string'."
  (let ((s (gensym))
        (n (gensym)))
    `(let ((,s ,string)
           (,n ,num))
       (when (and (stringp ,s)
                  (string-match ,regexp ,s ,start)
                  (match-beginning ,n))
         (substring ,s (match-beginning ,n) (match-end ,n))))))


(defmacro split-string* (string &optional separators omit-nulls trim)
  "Split STRING into substrings bounded by match for SEPARATORS.

Like `split-string' in Emacs 24.4+
Optional argument OMIT-NULLS omit null strings.
Optional argument TRIM regexp used to trim."
  (if-version%
      <= 24.4
      `(split-string ,string ,separators ,omit-nulls ,trim)
    (let ((s (gensym))
          (p (gensym))
          (d (gensym)))
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


 ;; end of Strings


;; Platform Related Functions


(defmacro posix-path (path)
  "Transpose PATH to posix path."
  (let ((p (gensym)))
    `(let ((,p ,path))
       (when (stringp ,p)
         (if (string-match "^\\([A-Z]:\\)" ,p)
             (replace-regexp-in-string "\\\\"
                                       "/"
                                       (replace-match
                                        (downcase (match-string 1 ,p))
                                        t t ,p))
           ,p)))))


(defmacro shell-command* (command &rest args)
  "Return a cons cell (code . output) after execute COMMAND in inferior shell.

See `shell-command' and `shell-command-to-string' for details.

If you want to set the environment temporarily that
`shell-command*' run in:
 (let ((process-environment (cons \"GREP_OPTIONS=--color=always\"
                                   process-environment)))
   (shell-command* \"echo 'a' | grep 'a'\"))
Optional argument ARGS for COMMAND."
  (declare (indent 1))
  (let ((cmd (gensym))
        (buf (gensym)))
    `(let ((,cmd ,command)
           (,buf (generate-new-buffer (symbol-name (gensym "sc")))))
       (with-current-buffer ,buf
         (cons (let ((x (call-process
                         shell-file-name nil ,buf nil
                         shell-command-switch
                         (mapconcat #'identity
                                    (cons ,command (list ,@args)) " "))))
                 (cond ((integerp x) x)
                       ((string-match "^.*\\([0-9]+\\).*$" x)
                        (match-string 1 x))
                       (t -1)))
               (let ((s (buffer-string)))
                 (and (buffer-name ,buf) (kill-buffer ,buf))
                 (if (string= "\n" s) nil s)))))))


(defmacro executable-find% (command &optional check)
  "Find and check COMMAND at compile time.

Return nil if no COMMAND found or CHECK failed.
Return the first match, if multiple COMMANDs had been found
or the one that CHECK return t."
  (if check
      (let ((cmd (shell-command* (if-platform% 'windows-nt
                                     "where"
                                   "command -v")
                   (funcall `(lambda () ,command)))))
        (when (zerop (car cmd))
          (let* ((ss (cdr cmd))
                 (path (split-string* ss "\n" t))
                 (p (cond
                     ((and (consp path) (functionp check))
                      (catch 'check
                        (dolist* (x path)
                          (when (funcall check
                                         (shell-quote-argument
                                          (posix-path x)))
                            (throw 'check x)))
                        nil))
                     ((consp path) (car path))
                     (t path))))
            (posix-path p))))
    (let ((path (executable-find (funcall `(lambda () ,command)))))
      (ignore* check)
      `,path)))


 ;; end of Platform Related Functions




(provide 'fn)

;;; fn.el ends here
