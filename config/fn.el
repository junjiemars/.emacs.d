;;; fn.el --- primitive functions -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;;; Commentary:
;;
;;; Code:


;;; alias

(fset 'range #'number-sequence)

(unless-fn% 'char= nil
  (fset 'char= #'char-equal))

(unless-fn% 'characterp nil
  (fset 'characterp #'char-valid-p))

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

;; end of alias


;;; general function/macro

(defun flatten (seq)
  "Flatten SEQ."
  (cond ((atom seq) (list seq))
        ((null (cdr seq)) (flatten (car seq)))
        (t (append (flatten (car seq)) (flatten (cdr seq))))))


(unless-fn% 'take nil
  (defun take (n seq)
    "Return a sequence of the first N items in SEQ."
    (let ((s))
      (while (and (> n 0) seq)
        (setq s (cons (car seq) s)
              n (1- n)
              seq (cdr seq)))
      (nreverse s))))


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
  (let ((n1 (gensym*)))
    `(let ((,n1 ,newelt))
       (setq ,seq (append (if ,uniquely
                              (delete ,n1 ,seq)
                            ,seq)
                          (list ,n1))))))


(defmacro seq-ins! (newelt seq idx)
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


;;;;
;; common lisp macro
;;;;


;; Load cl-lib/cl at runtime
(if-version% <= 24.1 (require 'cl-lib)
  (let ((byte-compile-warnings nil))
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


(defmacro loop* (&rest clause)
  "The Common Lisp \\=`loop\\=' macro.\n
Optional argument CLAUSE such as for clause, iteration clause,
accumulate clause and Miscellaneous clause."
  (if-fn% 'cl-loop 'cl-lib
          `(cl-loop ,@clause)
    `(loop ,@clause)))


(defmacro time (&rest form)
  "Execute FORM and print timing information on *message*."
  (declare (indent 0))
  (let ((b (gensym*)))
    `(let ((,b (current-time)))
       (prog1 (progn ,@form)
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
    (list 'prog1 nil (cons 'list `,@vars))))


(defun true (&rest x)
  "Return true value ignore X."
  (prog1 t (ignore* x)))


(defmacro defmacro-if-feature% (feature)
  "Define if-FEATURE% compile-time macro."
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


(defmacro make-thread* (fn &optional join name)
  "Threading call FN with NAME or in JOIN mode."
  `(if-fn% 'make-thread nil
           (if% ,join
               (thread-join (make-thread ,fn ,name))
             (make-thread ,fn ,name))
     (ignore* ,join ,name)
     (funcall ,fn)))


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

;;;
;; Strings
;;;


(defun strchr (str chr)
  "Return the index of the located CHR of STR from left side."
  (let ((i 0) (l (length str)))
    (catch 'break
      (while (< i l)
        (when (= chr (aref str i))
          (throw 'break i))
        (setq i (1+ i))))))


(defun strrchr (str chr)
  "Return the index of the located CHR of STR from right side."
  (let* ((l (length str)) (i (1- l)))
    (catch 'break
      (while (>= i 0)
        (when (= chr (aref str i))
          (throw 'break i))
        (setq i (1- i))))))


(defmacro string-trim> (s &optional rr)
  "Remove tailing whitespaces or matching of RR at the end of S."
  (let ((s1 (gensym*))
        (r1 (gensym*)))
    `(let ((,s1 ,s))
       (when (stringp ,s1)
         (let ((,r1 (concat "\\(?:"
                            (or ,rr "[ \t\n\r]+\\'")
                            "\\)\\'")))
           (let ((i (string-match ,r1 ,s1 0)))
             (if i (substring-no-properties ,s1 0 i) ,s1)))))))


(defmacro string-trim< (s &optional lr)
  "Remove leading whitespaces or matching of LR from S."
  (let ((s1 (gensym*))
        (r1 (gensym*)))
    `(let ((,s1 ,s))
       (when (stringp ,s1)
         (let ((,r1 (concat "\\`\\(?:"
                            (or ,lr "\\`[ \t\n\r]+")
                            "\\)")))
           (if (string-match ,r1 ,s1)
               (substring-no-properties ,s1 (match-end 0))
             ,s1))))))


(defmacro string-trim>< (s &optional rr lr)
  "Remove leading and trailing whitespaces or matching of LR/RR from S."
  (let ((s1 (gensym*))
        (r1 (gensym*)))
    `(let* ((,r1 ,rr)
            (,s1 (string-trim> ,s ,r1)))
       (string-trim< ,s1 ,lr))))


(defmacro string-match* (regexp string num &optional start)
  "Return string of text match for REGEXP in STRING.\n
Return nil if NUMth pair didn’t match, or there were less than NUM pairs.
NUM specifies which parenthesized expression in the REGEXP.
If START is non-nil, start search at that index in STRING.\n
See \\=`string-match\\=' and \\=`match-string\\='."
  (let ((s (gensym*))
        (n (gensym*))
        (b (gensym*)))
    `(let* ((,s ,string)
            (,n ,num)
            (,b (and (stringp ,s) (string-match ,regexp ,s ,start)
                     (match-beginning ,n))))
       (when ,b
         (substring-no-properties ,s ,b (match-end ,n))))))


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


;; end of Strings


;;; read/save-str/sexp-file

(defmacro save-sexp-to-file (sexp file)
  "Save SEXP to FILE.\n
Returns the name of FILE when successed otherwise nil."
  (let ((s (gensym*))
        (f (gensym*))
        (b (gensym*)))
    `(let ((,s ,sexp)
           (,f ,file)
           (,b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
           (lexical-let%
               ((format-alist nil)
                (coding-system-for-write 'no-conversion))
             (with-current-buffer ,b
               (prin1 ,s ,b)
               (write-region (point-min) (point-max) ,f)
               ,f))
         (and (buffer-name ,b) (kill-buffer ,b))))))


(defmacro read-sexp-from-file (file)
  "Read the first sexp from FILE."
  (let ((f (gensym*))
        (b (gensym*)))
    `(let ((,f ,file))
       (when (and (stringp ,f) (file-exists-p ,f))
         (let ((,b (get-buffer-create* (symbol-name (gensym*)) t)))
           (unwind-protect
               (with-current-buffer ,b
                 (insert-file-contents-literally ,f)
                 (read ,b))
             (and (buffer-name ,b) (kill-buffer ,b))))))))


(defmacro save-str-to-file (str file)
  "Save STR to FILE.\n
Returns the name of FILE when successed otherwise nil."
  (let ((s (gensym*))
        (f (gensym*))
        (b (gensym*)))
    `(let ((,s ,str)
           (,f ,file)
           (,b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
           (lexical-let%
               ((format-alist nil)
                (coding-system-for-write 'no-conversion))
             (with-current-buffer ,b
               (insert ,s)
               (write-region (point-min) (point-max) ,f)
               ,f))
         (and (buffer-name ,b) (kill-buffer ,b))))))


(defmacro read-str-from-file (file)
  "Read string from FILE."
  (let ((f (gensym*))
        (b (gensym*)))
    `(let ((,f ,file))
       (when (and (stringp ,f) (file-exists-p ,f))
         (let ((,b (get-buffer-create* (symbol-name (gensym*)) t)))
           (unwind-protect
               (with-current-buffer ,b
                 (insert-file-contents-literally ,f)
                 (buffer-substring-no-properties (point-min) (point-max)))
             (and (buffer-name ,b) (kill-buffer ,b))))))))

;; end of read/save-str/sexp-file


;;; Platform Related Functions

(defmacro file-name-base* (path)
  "Return base name of PATH."
  `(file-name-sans-extension (file-name-nondirectory ,path)))


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
  "Return a cons cell (code . output) after execute COMMAND in inferior shell.\n
See \\=`shell-command\\=' and \\=`shell-command-to-string\\=' for details.\n
If you want to set the environment temporarily that
\\=`shell-command*\\=' run in:
 (let ((process-environment (cons \"GREP_OPTIONS=--color=always\"
                                   process-environment)))
   (shell-command* \"echo \\='a\\=' | grep \\='a\\='\"))
Optional argument ARGS for COMMAND."
  (declare (indent 1))
  (let ((c1 (gensym*))
        (b (gensym*)))
    `(let ((,c1 ,command)
           (,b (get-buffer-create* (symbol-name (gensym*)) t)))
       (unwind-protect
           (with-current-buffer ,b
             (cons (let ((x (call-process
                             shell-file-name nil ,b nil
                             shell-command-switch
                             (mapconcat #'identity
                                        (cons ,c1 (list ,@args)) " "))))
                     (cond ((integerp x) x)
                           ((string-match "^.*\\([0-9]+\\).*$" x)
                            (match-string-no-properties 1 x))
                           (t -1)))
                   (let ((s (buffer-substring-no-properties
                             (point-min) (point-max))))
                     (if (string= "\n" s) nil s))))
         (and (buffer-name ,b) (kill-buffer ,b))))))


(defmacro executable-find% (command &optional fn)
  "Return the path of COMMAND at compile time.\n
Return nil if no COMMAND found.
If FN is nil then return the path, otherwise call FN with the path."
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


;; end of Platform Related Functions


(provide 'fn)


;; end of fn.el
