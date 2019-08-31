;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; basic.el
;;;;



;; `load-path' versioned dirs
(add-to-list 'load-path (v-home% "config/") nil #'string=)
(add-to-list 'load-path (v-home% "private/") nil #'string=)




;; Versioned Dirs: .*

;; Auto-save
(setq auto-save-list-file-prefix (v-home! ".save/auto-"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(v-home! ".backup/"))))

;; eww bookmarks
(when-version%
    <= 24.4
  (setq% eww-bookmarks-directory (v-home! ".bookmarks/") 'eww))

;; Bookmark: file in which to save bookmarks
(setq% bookmark-default-file
       (v-home! ".bookmarks/emacs.bmk") 'bookmark)

;; Eshell
(setq% eshell-directory-name (v-home! ".eshell/") 'eshell)

;; Games: a directory for game scores
(setq% gamegrid-user-score-file-directory (v-home! ".games/") 'gamegrid)

;; Ido saved state between invocations
(setq% ido-save-directory-list-file (v-home! ".ido/ido.last") 'ido)

;; Image dired: where thumbnail images are stored.
(setq% image-dired-dir (v-home! ".dired/image/") 'image-dired)

;; Savehist: save minibuffer history
(setq% savehist-file (v-home! ".minibuffer/history") 'savehist)

;; Recentf: save the recent list into
(setq% recentf-save-file (v-home! ".recentf/recentf") 'recentf)

;; Rmail
(setq rmail-file-name (v-home! ".mail/RMAIL"))

;; Nsm: Network Security Manager
(setq% nsm-settings-file (v-home! ".nsm/security.data") 'nsm)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(setq% save-place-file (v-home! ".places/places") 'saveplace)

;; Server
(setq% server-auth-dir (v-home! ".server/") 'server)

;; Semantic
(when-version% <= 23
  (setq% semanticdb-default-save-directory
         (v-home! ".semantic/db/") 'semantic/db-file))

;; Tramp
(when-version% <= 23
  (setq% tramp-persistency-file-name (v-home! ".tramp/tramp")
         (if-version% > 24 'tramp
           'tramp-cache)))

;; Url
(setq% url-configuration-directory (v-home! ".url/") 'url)


 ;; Versioned Dirs


;; Platform Related Functions

(when-platform% 'windows-nt
  
  (defmacro windows-nt-posix-path (path)
    "Return posix path from Windows PATH which can be recognized on`system-type'."
    `(when (stringp ,path)
       (if (string-match "^\\([A-Z]:\\)" ,path)
           (replace-regexp-in-string
            "\\\\" "/"
            (replace-match (downcase (match-string 1 ,path)) t t ,path))
         ,path))))


 ;; end of Platform Related Functions


;; Compatible Macro

(defmacro assoc** (key list &optional testfn)
  "Return non-nil if KEY is equal to the `car' of an element of LIST.

The value is actually the first element of LIST whose car equals KEY.
Equality is defined by TESTFN if non-nil or by `equal' if nil."
  (if-version%
      <= 26.1
      `(assoc ,key ,list ,testfn)
    (if-fn% 'cl-assoc 'cl-lib
            ;; `cl-assoc' autoloaded, but may not autoload
            `(progn
               (require 'cl-lib)
               (cl-assoc ,key ,list :test (or ,testfn #'equal)))
      (when-fn% 'assoc* 'cl
        `(with-no-warnings
           (require 'cl)
           (assoc* ,key ,list :test (or ,testfn #'equal)))))))


(defmacro alist-get* (key alist &optional default remove testfn)
  "Return the value associated with KEY in ALIST.

If KEY is not found in ALIST, return DEFAULT.
Use TESTFN to lookup in the alist if non-nil, otherwise use `equal'."
  (if-version%
      <= 26.1
      `(alist-get ,key ,alist ,default ,remove ,testfn)
    (ignore* remove) ;;silence byte-compiler.
    `(let ((x (assoc** ,key ,alist ,testfn)))
       (if (consp x)
           (cdr x)
         ,default))))


;; Unifiy `cl-remove' and `remove*'
(defmacro remove** (item seq &rest keys)
  "Remove all occurrences of ITEM in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ.
\nKeywords supported:  :test :test-not :key :count :start :end :from-end
\n(fn ITEM SEQ [KEYWORD VALUE]...)"
  (if-fn% 'cl-remove 'cl-lib
          ;; `cl-remove' autoloaded
          `(cl-remove ,item ,seq ,@keys)
    (when-fn% 'remove* 'cl
      `(with-no-warnings
         (progn
           (require 'cl)
           (remove* ,item ,seq ,@keys))))))

(defmacro remove-if* (predicate seq &rest keys)
  "Remove all items satisfying PREDICATE in SEQ.
This is a non-destructive function; it makes a copy of SEQ if necessary
to avoid corrupting the original SEQ."
  (declare (indent 2))
  (if-fn% 'cl-remove-if 'cl-lib
          `(cl-remove-if ,predicate ,seq ,@keys)
    (when-fn% 'remove-if 'cl
      `(with-no-warnings
         (progn
           (require 'cl)
           (remove-if ,predicate ,seq ,@keys))))))


;; Unify `cl-member' and `member*'
(defmacro member** (item list &rest keys)
  "Find the first occurrence of ITEM in LIST.
Return the sublist of LIST whose car is ITEM.
\nKeywords supported:  :test :test-not :key
\n(fn ITEM LIST [KEYWORD VALUE]...)"
  (if-version%
      > 24
      `(with-no-warnings
         (require 'cl)
         (member* ,item ,list ,@keys))
    ;; `cl-member' autoloaded
    `(cl-member ,item ,list ,@keys)))


(defmacro every* (pred &rest seq)
  "Return t if PRED is t of every element of SEQ."
  (declare (indent 1))
  (if-fn% 'cl-every 'cl-lib
          `(cl-every ,pred (list ,@seq))
    (when-fn% 'every 'cl
      `(with-no-warnings
         (require 'cl)
         (every ,pred (list ,@seq))))))


(unless-fn% 'characterp nil
  (defalias 'characterp #'char-valid-p))


 ;; end of Compatible Macro


;; Strings

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
like `split-string' Emacs 24.4+"
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


(defmacro string=* (s1 s2 &rest ss)
  "Return t if S1, S2 and SS have identical contents."
  (declare (indent 2))
  `(and (string= ,s1 ,s2)
        (every* (lambda (x) (string= ,s2 x)) ,@ss)))

(defmacro char=* (c1 c2 &rest cc)
  "Return t if C1, C2 and CC are identical character."
  (declare (indent 2))
  `(and (char-equal ,c1 ,c2)
        (every* (lambda (x) (char-equal ,c2 x)) ,@cc)))

 ;; end of Strings


;; Compatible Functions

(unless-fn% 'with-eval-after-load nil
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See ‘eval-after-load’
for more details about the different forms of FILE and their semantics."
    (declare (indent 1))
    `(eval-after-load ,file
       `(funcall ,(lambda () ,@body)))))


(defmacro defcustom% (symbol standard doc &rest args)
  "Declare SYMBOL as a customizable variable with the STANDARD value.
STANDARD should be computed at compile-time. In `defcustom'
STANDARD always be computed at runtime whatever the current
`lexical-binding' is."
  (declare (doc-string 3) (debug (name body)))
  (let ((-standard- (funcall `(lambda () ,standard))))
    `(custom-declare-variable
      ',symbol
      ',-standard-
      ,doc
      ,@args)))


(defmacro region-active-if (then &rest else)
  "If `region-active-p' or `mark-active' is t do THEN, otherwise do ELSE..."
  (declare (indent 1))
  `(if-fn% 'region-active-p nil
           (if (region-active-p)
               ,then
             (progn% ,@else))
     (if mark-active
         ,then
       (progn% ,@else))))


 ;; end of Compatible Functions


;; File Functions

(defun save-sexp-to-file (sexp file)
  "Save SEXP to FILE. 

Returns the name of FILE when successed otherwise nil."
  (when (and (save-excursion
               (let ((sexp-buffer (find-file-noselect file)))
                 (set-buffer sexp-buffer)
                 (erase-buffer)
                 (print sexp sexp-buffer)
                 (save-buffer)
                 (kill-buffer sexp-buffer)))
             (file-exists-p file))
    file))


(defun save-str-to-file (str file)
  "Save STR to FILE. 

Returns the name of FILE when successed otherwise nil."
  (progn
    (with-temp-file file (insert str))
    (when (file-exists-p file)
      file)))


(defun read-str-from-file (file)
  "Read string from FILE."
  (when (and (stringp file) (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))


(define-hash-table-test 'string-hash= #'string= #'sxhash)

(defmacro save-hash-table-to-file (var table file &optional test)
  "Save the TABLE that referenced by VAR to FILE.

TEST is the symbol of hash testing, default is `eql'.
See also`define-hash-table-test'."
  `(if% (let ((tbl (make-hash-table :test #'eql)))
          (ignore* test)
          (puthash 1 11 tbl)
          (string-match "(1 11)" (prin1-to-string tbl)))
       (save-sexp-to-file
        `(set ',,var ,,table)
        ,file)
     (let ((lst nil))
       (maphash (lambda (k v)
                  (push (list k v) lst))
                ,table)
       (save-sexp-to-file
        `(let ((tbl (make-hash-table :test (or ',,test #'eql))))
           (mapc (lambda (x)
                   (puthash (car x) (cadr x) tbl))
                 ',lst)
           (set ',,var tbl))
        ,file))))


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
  (and (stringp file)
       (file-name-directory (directory-file-name file))))


(defmacro file-symlink-p* (file)
  "Return the link target as a string if FILE is the name of a symbolic link."
  `(file-symlink-p (if (directory-name-p ,file)
                       (directory-file-name ,file)
                     ,file)))


(defmacro file-in-dirs-p (file dirs)
  "Return t if FILE in DIRS, otherwise nil."
  `(member** (file-name-directory ,file)
             ,dirs
             :test (lambda (a b)
                     (let ((case-fold-search
                            (when-platform% 'windows-nt t)))
                       (string-match (string-trim> b "/")
                                     a)))))


(defun dir-iterate (dir ff df fn dn)
  "Iterate DIR.

Starting at DIR, look down directory hierarchy for maching FF or
DF. Ignores the symbol links of pointing itself or up directory.

FF specify file-filter (lambda (file-name absolute-name)...), if
FF return non-nil then call FN.

DF specify dir-filter (lambda (dir-name absolute-name)...), if DF
return non-nil then call DN.

FN specify file-function (lambda (absolute-name)...), process
filted files.

DN specify dir-function (lambda (aboslute-name)...), process
filted directories."
  (let ((files (remove-if*
                   (lambda (x)
                     (or (string= "./" x)
                         (string= "../" x)))
                   (file-name-all-completions "" dir))))
    (while files
      (let ((f (car files)))
        (let ((a (expand-file-name f dir)))
          (if (directory-name-p f)
              (when (and (let ((ln (file-symlink-p* a)))
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
  "Backtrack DIR.

Starting at DIR, look up directory hierarchy for prefered
directory or file. Ignores the symbol links of directory.

PREFER (lambda (dir files)...)."
  (let ((d (expand-file-name (if (directory-name-p dir)
                                 dir
                               (file-name-directory dir))))
        (stop "\\(^/\\|[a-zA-Z]:/\\)\\'"))
    (while (and (stringp d)
                (directory-name-p d)
                (not (string-match stop d)))
      (and prefer (funcall prefer d
                           (remove-if*
                               (lambda (x)
                                 (or (string= "./" x)
                                     (string= "../" x)
                                     (let ((dx (concat d x)))
                                       (and (directory-name-p dx)
                                            (file-symlink-p* dx)))))
                               (file-name-all-completions "" d))))
      (setq d (path- d)))))


(defmacro shell-command* (command &rest args)
  "Return a cons cell (code . output) after execute COMMAND in inferior shell.

See `shell-command' and `shell-command-to-string' for details.

If you want to set the environment temporarily that
shell-command* run in:
 (let ((process-environment (cons \"GREP_OPTIONS=' --color=always'\" process-environment)))
   (shell-command* \"echo 'a' | grep 'a'\")) "
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


(defmacro remote-norm-file (file)
  "Return an identification when FILE specifies a location on a remote system.

On ancient Emacs, `file-remote-p' will return a vector."
  `(match-string* "^\\(/sshx?:[_-a-zA-Z0-9]+@?[_-a-zA-Z0-9]+:\\)"
                  ,file 1))

(defmacro remote-norm-id (remote)
  "Norm the REMOTE to '(method {user | id} [host])' form."
  `(when (stringp ,remote)
     (split-string* ,remote "[:@]" t "/")))

(defmacro remote-norm->user@host (remote)
  "Norm the REMOTE to '{user | id}[@host]' form."
  `(let ((rid (remote-norm-id ,remote)))
     (when (consp rid)
       (concat (cadr rid) (when (caddr rid)
                            (concat "@" (caddr rid)))))))


 ;; end of File functions


;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files except current `emacs-version'.

Clean all when ALL is t,
otherwise default to keep the directories of current `emacs-version'."
  (let ((dirs (list `,(emacs-home* ".save/")
                    `,(emacs-home* ".backup/")
                    `,(emacs-home* ".bookmarks/")
                    `,(emacs-home* ".desktop/")
                    `,(emacs-home* ".eshell/")
                    `,(emacs-home* ".exec/")
                    `,(emacs-home* ".games/")
                    `,(emacs-home* ".ido/")
                    `,(emacs-home* ".dired/")
                    `,(emacs-home* ".minibuffer/")
                    `,(emacs-home* ".nsm/")
                    `,(emacs-home* ".places/")
                    `,(emacs-home* ".recentf/")
                    `,(emacs-home* ".semantic/")
                    `,(emacs-home* ".server/")
                    `,(emacs-home* ".tags/")
                    `,(emacs-home* ".tramp/")
                    `,(emacs-home* ".url/"))))
    (dolist* (d dirs)
      (when (file-exists-p d)
        (dolist* (f (directory-files d nil "^[gt]_.*$"))
          (when (or all
                    (not (string-match
                          (concat "^[gt]_" emacs-version) f)))
            (message "#Clean saved user file: %s" (concat d f))
            (if-platform% 'windows-nt
                (shell-command (concat "rmdir /Q /S " (concat d f)))
              (shell-command (concat "rm -r " (concat d f))))))))))


(defun reset-emacs ()
  "Clean all compiled file and desktop, then restart Emacs."
  (progn
    (clean-saved-user-files t)
    (clean-compiled-files)
    (setq kill-emacs-hook nil)
    (kill-emacs 0)))


 ;; end of Emacs manipulation


(defmacro platform-arch ()
  "Return the platform architecture with (arch-str . num) cons cell."
  (let ((arch (if-platform% 'windows-nt
                  (getenv "PROCESSOR_ARCHITECTURE")
                (let ((m (shell-command* "uname -m")))
                  (when (zerop (car m)) (string-trim> (cdr m) "\n"))))))
    (when arch (if (string-match "[xX]86_64\\|[aA][mM][dD]64" arch)
                   `(cons ,arch 64)
                 `(cons ,arch 32)))))




;; Computations

(defalias 'range #'number-sequence)


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


 ;; end of Computations


;; define key macro

(defmacro if-key% (keymap key test-def then &rest else)
  "If TEST-DEF for KEY in KEYMAP do then, otherwise do ELSE..."
  (declare (indent 4))
  `(if% (funcall ,test-def (lookup-key ,keymap ,key))
       ,then
     ,@else))

(defmacro define-key% (keymap key def)
  "Define KEY to DEF in KEYMAP when the KEY binding of DEF is not exists."
  `(if-key% ,keymap ,key
            (lambda (d) (not (eq d ,def)))
            (define-key ,keymap ,key ,def)))

 ;; End of key macro


(defmacro safe-local-variable* (var)
  "Safe local VAR in -*- line, see `enable-local-variables'"
  `(put ,var 'safe-local-variable
        (lambda (x) (ignore* x) t)))

;; linum mode, requires Emacs-23.1+
(defmacro-if-feature% linum)

;; semantic, require Emacs-24.4+
(defmacro-if-feature% semantic)

;; default web browser: eww, requires Emacs-24.4+
(defmacro-if-feature% eww)

;; socks
(defmacro-if-feature% socks
  (and (require 'url-vars nil t)
       (require 'socks nil t)
       (boundp 'url-gateway-method)
       (boundp 'socks-server)))


(if-feature-socks%
  
  (defun toggle-socks! (&optional arg)
    "Toggle `url-gatewary-method' to socks or native.
With prefix argument ARG, `url-gatewary-method' via socks if ARG is 
positive, otherwise via native."
    (interactive "P")
    (let ((native (lambda ()
                    (setq% url-gateway-method 'native 'url-vars)
                    (setq% socks-server
                           (list "Default server"
                                 nil
                                 (self-spec->*env-spec :socks :port)
                                 (self-spec->*env-spec :socks :version))
                           'socks)))
          (socks (lambda ()
                   (setq% url-gateway-method 'socks 'url-vars)
                   (setq% socks-server
                          (list "Default server"
                                (self-spec->*env-spec :socks :server)
                                (self-spec->*env-spec :socks :port)
                                (self-spec->*env-spec :socks :version))
                          'socks))))
      ;; (require 'url)
      (if (null arg)
          (if (eq url-gateway-method 'native) (funcall socks) (funcall native))
        (funcall socks))
      (message "socks%s as url gateway %s"
               (list (self-spec->*env-spec :socks :server)
                     (self-spec->*env-spec :socks :port)
                     (self-spec->*env-spec :socks :version))
               (if (eq url-gateway-method 'native)
                   "disabled"
                 "enabled")))))

(if-feature-socks%
  (when (self-spec->*env-spec :socks :allowed)
    (toggle-socks! t)))

 ;; end of Socks functions


 ;; end of basic.el
