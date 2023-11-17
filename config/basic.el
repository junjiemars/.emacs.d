;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; basic.el
;;;;



;;; `load-path' versioned dirs
(push! (v-home% "config/") load-path)
(push! (v-home% "private/") load-path)




;;; Versioned Dirs: .*


;; `abbrev'
(setq% abbrev-file-name (v-home! ".abbrev/defs") 'abbrev)

;; auto-save
(setq% auto-save-list-file-prefix (v-home! ".save/auto-"))


;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq% backup-directory-alist `(("." . ,(v-home! ".backup/"))))

;; `calc'
(setq% calc-settings-file (v-home! ".calc/calc.el") 'calc)

;; `eww' bookmarks
(setq% eww-bookmarks-directory (v-home! ".bookmarks/") 'eww)

;; `bookmark': file in which to save bookmarks
(setq% bookmark-default-file
       (v-home! ".bookmarks/emacs.bmk") 'bookmark)

;; `eshell'
(setq% eshell-directory-name (v-home! ".eshell/") 'eshell)

;; `gamegrid': a directory for game scores
(setq% gamegrid-user-score-file-directory (v-home! ".games/") 'gamegrid)

;; `ido' saved state between invocations
(setq% ido-save-directory-list-file (v-home! ".ido/ido.last") 'ido)

;; `image-dired': where thumbnail images are stored
(setq% image-dired-dir (v-home! ".dired/image/") 'image-dired)

;; `multisession': where multisession variables stored
(setq% multisession-directory (v-home! ".multisession/") 'multisession)

;; `nsm': Network Security Manager
(setq% nsm-settings-file (v-home! ".nsm/security.data") 'nsm)

;; `project'
(setq% project-list-file (v-home! ".project/list") 'project)

;; Savehist: save minibuffer history
(setq% savehist-file (v-home! ".minibuffer/history") 'savehist)

;; `recentf': save the recent list into
(setq% recentf-save-file (v-home! ".recentf/recentf") 'recentf)

;; `rmail'
(setq% rmail-file-name (v-home! ".mail/RMAIL") 'rmail)

;; `saveplace'
;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(setq% save-place-file (v-home! ".places/places") 'saveplace)

;; `server'
(setq% server-auth-dir (v-home! ".server/") 'server)

;; `tramp'
(setq% tramp-persistency-file-name (v-home! ".tramp/tramp")
       (if-version% > 24 'tramp
             'tramp-cache))

;; `transient'
(setq% transient-save-history nil 'transient)

;; `url'
(setq% url-configuration-directory (v-home! ".url/") 'url)


 ;; Versioned Dirs


;;; Compatible Macros

(unless-fn% 'user-error nil
  (defun user-error (format &rest args)
    "Signal a pilot error."
    (signal 'user-error (list (apply #'format-message format args)))))


(defmacro called-interactively-p* (&optional kind)
  "Return t if the containing function was called by
`call-interactively'."
  (if-fn% 'called-interactively-p nil
          `(called-interactively-p ,kind)
    (ignore* kind)
    `(interactive-p)))


(eval-and-compile
  (unless-fn% 'declare-function nil
    (defmacro declare-function (&rest _))))

;; end of Compatible Macros


;;; Compatible Functions

(unless-fn% 'with-eval-after-load nil
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.

FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature.  See \\=`eval-after-load\\='
for more details about the different forms of FILE and their semantics."
    (declare (indent 1))
    `(eval-after-load ,file
       `(funcall ,(lambda () ,@body)))))


(defmacro defcustom% (symbol standard doc &rest args)
  "Declare SYMBOL as a customizable variable with the STANDARD value.
STANDARD should be computed at compile-time. In \\=`defcustom\\='
STANDARD always be computed at runtime whatever the current
\\=`lexical-binding\\=' is."
  (declare (doc-string 3) (debug (name body)))
  (let ((-standard- (funcall `(lambda () ,standard))))
    `(custom-declare-variable
      ',symbol
      ',-standard-
      ,doc
      ,@args)))


(defmacro region-active-if (then &rest else)
  "If \\=`region-active-p\\=' or \\=`mark-active\\=' is t do THEN,
otherwise do ELSE..."
  (declare (indent 1))
  `(if mark-active
       ,then
     (progn% ,@else)))

(defmacro region-active-unless (&rest then)
  "Unless \\=`region-active-p\\=' or \\=`mark-active\\=' is t do THEN."
  (declare (indent 0))
  `(region-active-if nil
     ,@then))


 ;; end of Compatible Functions



(define-hash-table-test 'string-hash= #'string= #'sxhash)

;; (defmacro save-hash-table-to-file (var table file &optional test)
;;   "Save the TABLE that referenced by VAR to FILE.

;; TEST is the symbol of hash testing, default is `eql'.
;; See also`define-hash-table-test'."
;;   `(if% (let ((tbl (make-hash-table :test #'eql)))
;;           (ignore* test)
;;           (puthash 1 11 tbl)
;;           (string-match "(1 11)" (prin1-to-string tbl)))
;;        (save-sexp-to-file
;;         `(set ',,var ,,table)
;;         ,file)
;;      (let ((lst nil))
;;        (maphash (lambda (k v)
;;                   (push (list k v) lst))
;;                 ,table)
;;        (save-sexp-to-file
;;         `(let ((tbl (make-hash-table :test (or ',,test #'eql))))
;;            (mapc (lambda (x)
;;                    (puthash (car x) (cadr x) tbl))
;;                  ',lst)
;;            (set ',,var tbl))
;;         ,file))))


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
  (let ((f (gensym)))
    `(let ((,f ,file))
       (and (stringp ,f)
            (file-name-directory (directory-file-name ,f))))))


(defmacro path-depth (path &optional separator)
  "Return the depth of PATH."
  (let ((p (gensym))
        (s (gensym)))
    `(let ((,p ,path)
           (,s (or ,separator "/")))
       (if (stringp ,p)
           (cond ((string= "" ,p) 0)
                 ((string= ,s ,p) 1)
                 (t (length (split-string* ,p ,s nil))))
         0))))


(defmacro file-in-dirs-p (file dirs)
  "Return t if the name of FILE matching DIRS, otherwise nil."
  (let ((f (gensym))
        (ds (gensym)))
    `(let ((,f ,file)
           (,ds ,dirs))
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
Starting at DIR, look down directory hierarchy for maching FF or
DF. Ignores the symbol links of pointing itself or up directory.\n
FF specify file-filter (lambda (file-name absolute-name)...), if
FF return non-nil then call FN.\n
DF specify dir-filter (lambda (dir-name absolute-name)...), if DF
return non-nil then call DN.\n
FN specify file-function (lambda (absolute-name)...), process
filted files.\n
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
                                         (file-symlink-p dx)))))
                            (file-name-all-completions "" d))))
      (setq d (path- d)))))


(defmacro remote-norm-file (file)
  "Return an identification when FILE specifies a location on a remote system.

On ancient Emacs, \\=`file-remote-p\\=' will return a vector."
  `(string-match* "^\\(/sshx?:[_-a-zA-Z0-9]+@?[_-a-zA-Z0-9]+:\\)"
                  ,file 1))

(defmacro remote-norm-id (remote)
  "Norm the REMOTE to (method {user | id} [host]) form."
  (let ((r (gensym)))
    `(let ((,r ,remote))
       (and (stringp ,r) (split-string* ,r "[:@]" t "/")))))

(defmacro remote-norm->user@host (remote)
  "Norm the REMOTE to {user | id}[@host] form."
  (let ((rid (gensym)))
    `(let ((,rid (remote-norm-id ,remote)))
       (when (consp ,rid)
         (concat (cadr ,rid) (when (car (cddr ,rid))
                               (concat "@" (car (cddr ,rid)))))))))


(defmacro url-retrieve* (url callback &optional cbargs silent inhibit-cookies)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished."
  (when-fn% 'url-retrieve 'url
    (if-version%
        <= 24
        `(url-retrieve ,url ,callback ,cbargs ,silent ,inhibit-cookies)
      (ignore* silent inhibit-cookies)
      `(url-retrieve ,url ,callback ,cbargs))))


(defun buffer-major-mode (&optional buffer-or-name)
  "Return \\=`major-mode\\=' associated with BUFFER-OR-NAME or current buffer."
  (buffer-local-value 'major-mode
                      (if buffer-or-name
                          (get-buffer buffer-or-name)
                        (current-buffer))))


 ;; end of file functions


;; Clean Emacs' user files

(defun clean-saved-user-files (&optional all)
  "Clean saved user files except current \\=`emacs-version\\='.

Clean all when ALL is t,
otherwise default to keep the directories of current \\=`emacs-version\\='."
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
  (let ((m64 "\\([xX]86_64\\|[aA][mM][dD]64\\|aarch64\\)")
        (bit (emacs-arch)))
    (if (string-match m64 system-configuration)
        `(cons ,(string-match* m64 system-configuration 1) ,bit)
      (if-platform% 'windows-nt
          (if (string-match m64 (getenv "PROCESSOR_ARCHITECTURE"))
              `(cons ,(getenv "PROCESSOR_ARCHITECTURE") ,bit)
            `(cons ,(getenv "PROCESSOR_ARCHITECTURE") ,bit))
        (let ((m (shell-command* "uname -m")))
          (if (and (zerop (car m))
                   (string-match m64 (string-trim> (cdr m) "\n")))
              `(cons ,(string-trim> (cdr m) "\n") ,bit)
            `(cons ,(string-trim> (cdr m) "\n") ,bit)))))))


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


(defmacro symbol@ (&optional thing)
  "Return the (cons \\='region|nil THING) at point."
  (let ((ss (gensym)))
    `(region-active-if
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
