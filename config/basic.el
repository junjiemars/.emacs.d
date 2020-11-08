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

;; calc
(setq% calc-settings-file (v-home! ".calc/calc.el") 'calc)

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
    "Return posix path from Windows PATH which can be recognized
on`system-type'."
    `(when (stringp ,path)
       (if (string-match "^\\([A-Z]:\\)" ,path)
           (replace-regexp-in-string
            "\\\\" "/"
            (replace-match (downcase (match-string 1 ,path)) t t ,path))
         ,path))))


 ;; end of Platform Related Functions

;;;;
;; Compatible Macro
;;;;

(defmacro user-error* (format &rest args)
  "Signal a pilot error."
  (declare (indent 1))
  (if-fn% 'user-error nil
          `(user-error , format ,@args)
    `(signal 'user-error
             (list (apply #'format ,format ,@args)))))


(defmacro called-interactively-p* (&optional kind)
  "Return t if the containing function was called by
`call-interactively'."
  (if-fn% 'called-interactively-p nil
          `(called-interactively-p ,kind)
    (ignore* kind)
    `(interactive-p)))


 ;; end of Compatible Macro


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
  (if-fn% 'region-active-p nil
          `(if (region-active-p)
               ,then
             (progn% ,@else))
    `(if mark-active
         ,then
       (progn% ,@else))))

(defmacro region-active-unless (&rest then)
  "Unless `region-active-p' or `mark-active' is t do THEN."
  (declare (indent 0))
  `(region-active-if nil
     ,@then))


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


(defun path-depth (path &optional separator)
  "Return the depth of PATH."
  (if (stringp path)
      (cond ((string= "" path) 0)
            ((string= (or separator "/") path) 1)
            (t (length (split-string* path (or separator "/") nil))))
    0))


(defmacro file-symlink-p* (file)
  "Return the link target as a string if FILE is the name of a symbolic link."
  `(file-symlink-p (if (directory-name-p ,file)
                       (directory-file-name ,file)
                     ,file)))


(defmacro buffer-file-name* (buffer)
  "Return the name of BUFFER only if BUFFER assocate with a file."
  (let ((b (gensym*)))
    `(when (bufferp ,buffer)
       (let ((,b (buffer-file-name ,buffer)))
         (when (stringp ,b)
           (substring-no-properties ,b))))))


(defmacro file-in-dirs-p (file dirs)
  "Return t if the name of FILE matching DIRS, otherwise nil."
  `(when (and (stringp ,file)
              (consp ,dirs))
     (let ((fname (file-name-directory ,file)))
       (some* (lambda (x)
                (let ((case-fold-search (when-platform%
                                            'windows-nt t)))
                  (when (stringp x)
                    (string-match (string-trim> x "/") fname))))
         ,dirs))))


(defmacro file-name-nondirectory% (filename)
  "Return file name FILENAME sans its directory at compile-time."
  (let* ((n (funcall `(lambda () ,filename)))
         (name (when n (file-name-nondirectory n))))
    `,name))


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
  (let ((files (remove-if* (lambda (x)
                             (or (null x)
                                 (string= "./" x)
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
                              (or (null x)
                                  (string= "./" x)
                                  (string= "../" x)
                                  (let ((dx (concat d x)))
                                    (and (directory-name-p dx)
                                         (file-symlink-p* dx)))))
                            (file-name-all-completions "" d))))
      (setq d (path- d)))))


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


(defmacro url-retrieve* (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished."
  (when-fn% 'url-retrieve 'url
    (if-version%
        <= 24
        `(url-retrieve ,url ,callback ,cbargs ,silent ,inhibit-cookies)
      (ignore* silent inhibit-cookies)
      `(url-retrieve ,url ,callback ,cbargs))))


 ;; end of file functions


;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files except current `emacs-version'.

Clean all when ALL is t,
otherwise default to keep the directories of current `emacs-version'."
  (interactive "P")
  (let ((dirs (list `,(emacs-home* ".backup/")
                    `,(emacs-home* ".bookmarks/")
                    `,(emacs-home* ".calc/")
                    `,(emacs-home* ".desktop/")
                    `,(emacs-home* ".dired/")
                    `,(emacs-home* ".eshell/")
                    `,(emacs-home* ".exec/")
                    `,(emacs-home* ".games/")
                    `,(emacs-home* ".ido/")
                    `,(emacs-home* ".minibuffer/")
                    `,(emacs-home* ".nsm/")
                    `,(emacs-home* ".places/")
                    `,(emacs-home* ".recentf/")
                    `,(emacs-home* ".save/")
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
  (interactive)
  (progn
    (clean-saved-user-files t)
    (clean-compiled-files)
    (setq kill-emacs-hook nil)
    (kill-emacs 0)))


 ;; end of Emacs manipulation


(defmacro platform-arch ()
  "Return platform architecture with (arch . bits) cons cell."
  (let ((b64 "\\([xX]86_64\\|[aA][mM][dD]64\\)"))
    (if (string-match b64 system-configuration)
        `(cons ,(match-string* b64 system-configuration 1) 64)
      (if-platform% 'windows-nt
          (if (string-match b64 (getenv "PROCESSOR_ARCHITECTURE"))
              `(cons ,(getenv "PROCESSOR_ARCHITECTURE") 64)
            `(cons ,(getenv "PROCESSOR_ARCHITECTURE") 32))
        (let ((m (shell-command* "uname -m")))
          (if (and (zerop (car m))
                   (string-match b64
                                 (string-trim> (cdr m) "\n")))
              `(cons ,(string-trim> (cdr m) "\n")
                     64)
            `(cons ,(string-trim> (cdr m) "\n")
                   32)))))))


(defmacro emacs-arch ()
  "Return emacs architecture, 64bits or 32bits."
  (if (= most-positive-fixnum (1- (expt 2 61)))
      64
    (if (= most-positive-fixnum (1- (expt 2 29)))
        32
      0)))


;; define key macro

(defmacro if-key% (keymap key test then &rest else)
  "If TEST function returns t for KEY in KEYMAP do then,
otherwise do ELSE..."
  (declare (indent 4))
  `(if% (funcall ,test (lookup-key ,keymap ,key))
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


(defmacro symbol@ (&optional thing)
  "Return the (cons 'region|nil THING) at point."
  `(region-active-if
       (let ((ss (buffer-substring-no-properties (region-beginning)
                                                 (region-end))))
         (setq mark-active nil)
         (cons 'region ss))
     (let ((ss (thing-at-point (or ,thing
                                   'symbol))))
       (and ss (cons nil (substring-no-properties ss))))))



 ;; end of basic.el
