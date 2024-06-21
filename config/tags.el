;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;
;; Commentary: tagging
;;;;

(defcustom%
 tags-program
 (list 'quote
       (eval-when-compile
         (or (executable-find%
              "ctags"
              (lambda (bin)
                (let ((ver (shell-command* bin "--version")))
                  (and (zerop (car ver))
                       (string-match "Exuberant Ctags [.0-9]+"
                                     (cdr ver))
                       ``(:bin "ctags" :cmd
                               ,(concat ,bin " -e %s -o %s -a %s"))))))
             (executable-find%
              "etags"
              (lambda (bin)
                (let ((ver (shell-command* bin "--version")))
                  (and (zerop (car ver))
                       (string-match "etags (GNU Emacs [.0-9]+)"
                                     (cdr ver))
                       ``(:bin "etags" :cmd
                               ,(concat ,bin " %s -o %s -a %s")))))))))
 "The tags program.\n
1st %s: ctags options;
2nd %s: explicit name of file for tag table;
3rd %s: append to existing tag file.\n
\\=`tags-table-list\\=' should be persitent between sessions
when \\=`desktop-globals-to-save\\=' include it."
 :type '(plist :key-type 'symbol :value-type 'string)
 :group 'tags)

(defalias '*tags*
  (lexical-let% ((b tags-program))
    (lambda (&optional n)
      (plist-get b (or n :bin))))
  "Tags' binary and options.")

(defalias 'tags-in-view-mode
  (lexical-let%
      ((b (delq nil
                (list (when (> (path-depth invocation-directory) 1)
                        (path- invocation-directory))
                      (when-package% package*-user-dir)
                      (v-home% "config/")
                      (v-home% "private/")
                      (v-home% "theme/")))))
    (lambda (&optional n)
      (cond (n (setq b (cons n b)))
            (t b))))
  "Tags buffer should open in \\=`view-mode\\='.")

(defvar *tags-option-history*
  (cond ((string= "ctags" (*tags*))
         `("--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq"
           "--langmap=c++:.h.cc--c++-kinds=+px"))
        ((string= "etags" (*tags*))
         `("-l c" "-l lisp" "-l auto"))
        (t nil))
  "Tags option history list.")

(defvar *tags-skip-history*
  (list "cpp\\|c\\+\\+\\|/python.*?/\\|/php.*?/\\|/ruby.*?/\\|/swift/")
  "Tags skip history list.")

(defalias 'mount-tags #'visit-tags-table
  "Mount existing TAGS into \\=`tags-table-list\\='.")

(defvar *tags-vcs-meta-dir*
  "^\\.git/$\\|\\.hg/$\\|\\.svn/$")

(defun unmount-tags (&optional tags)
  "Unmount TAGS from \\=`tags-table-list\\='."
  (interactive (list
                (unless (and current-prefix-arg
                             (yes-or-no-p "unmount all? "))
                  (fluid-let (file-name-history tags-table-list)
                    (if (consp file-name-history)
                        (read-file-name "unmount from ")
                      (user-error
                       "%s" "tags-table-list alreay empty"))))))
  (setq tags-file-name nil
        tags-table-list
        (when tags
          (let ((fn (expand-file-name tags)))
            (remove-if* (lambda (x) (string= x fn))
                        tags-table-list)))))

(defun dir-iterate (dir ff df fn dn)
  "Iterate DIR.\n
FF file-filter (lambda (file-name absolute-name)...),
DF dir-filter (lambda (dir-name absolute-name)...),
FN file-processor (lambda (absolute-name)...),
DN dir-processor (lambda (aboslute-name)...)."
  (inhibit-file-name-handler
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
          (setq files (cdr files)))))))

(defun dir-backtrack (dir prefer)
  "Backtrack DIR.\n
Starting at DIR, look up directory hierarchy for prefered
directory or file. Ignores the symbol links of directory.\n
PREFER (lambda (dir files)...)."
  (inhibit-file-name-handler
    (let ((d (expand-file-name
              (if (directory-name-p dir)
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
        (setq d (path- d))))))

(defun make-tags
    (home tags-file file-filter dir-filter &optional tags-option renew)
  "Make tags.\n
HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
TAGS-OPTION \\=`tags-program\\=' extra options,
RENEW overwrite the existing tags file when t else create it."
  (unless tags-program
    (signal 'void-variable (list 'tags-program tags-program)))
  (when (file-exists-p home)
    (let* ((tf (expand-file-name tags-file))
           (td (file-name-directory tf)))
      (if (file-exists-p tf)
          (when renew (delete-file tf))
        (path! td))
      (let ((h (propertize "make-tags" 'face 'minibuffer-prompt)))
        (dir-iterate
         home
         file-filter
         dir-filter
         (lambda (f)
           (let ((cmd (format (*tags* :cmd)
                              (or tags-option "")
                              tf
                              (shell-quote-argument f)
                              (shell-quote-argument f))))
             (message "%s %s... %s"
                      h cmd (if (zerop (car (shell-command* cmd)))
                                "ok"
                              "failed"))))
         nil)
        (message "%s for %s ... %s"
                 h tf (if (file-exists-p tf) "done" "failed"))))))


(defun make-c-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for C source code in HOME."
  (make-tags home
             tags-file
             (or file-filter
                 (lambda (f _) (string-match "\\.[ch]+$" f)))
             (or dir-filter
                 (lambda (d _)
                   (not
                    (string-match
                     (concat *tags-vcs-meta-dir* "\\|^out/$\\|^objs/$")
                     d))))
             option
             renew))


(defun make-lisp-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for Lisp source code in HOME."
  (make-tags home
             tags-file
             (or file-filter
                 (lambda (f _)
                   (string-match "\\.el$\\|\\.cl$\\|\\.lis[p]?$"
                                 f)))
             (or dir-filter
                 (lambda (d _)
                   (not
                    (string-match
                     (concat *tags-vcs-meta-dir* "\\|^out/$\\|^objs/$")
                     d))))
             option
             renew))


(defun make-nore-tags (&optional option renew)
  "Make tags for Nore \\=`emacs-home*\\=' directory."
  (interactive (list
                (read-string (concat (*tags*) " option: ")
                             (car *tags-option-history*)
                             '*tags-option-history*)
                (y-or-n-p "tags renew? ")))
  (make-lisp-tags (emacs-home*)
                  (tags-spec->% :nore)
                  option
                  (lambda (f _)
                    (string-match "\\.el$" f))
                  (lambda (d _)
                    (string-match "^config/$" d))
                  renew))


(defun make-emacs-tags (source &optional option renew)
  "Make tags for Emacs' C and Lisp SOURCE code."
  (interactive (list (read-directory-name "make tags for " source-directory)
                     (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (make-c-tags (concat source "src/")
               (tags-spec->% :emacs)
               option
               (lambda (f _) (string-match "\\.[ch]$" f))
               (lambda (_ __) t)
               renew)
  (make-lisp-tags (concat source "lisp/")
                  (tags-spec->% :emacs)
                  option
                  (lambda (f _) (string-match "\\.el$" f))
                  (lambda (_ __) t)))

;;

;;; `make-dir-tags'

(defun make-dir-tags
    (dir store &optional include-dir exclude-dir option renew)
  "Make tags for specified DIR."
  (interactive (list (read-directory-name "make tags for ")
                     (read-file-name "store tags in " nil nil nil ".tags")
                     (when current-prefix-arg
                       (read-string "tags include dir: " nil))
                     (when current-prefix-arg
                       (read-string "tags exclude dir: " nil))
                     (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (let ((home (path+ (expand-file-name dir)))
        (vcs *tags-vcs-meta-dir*)
        (exc (unless (string= exclude-dir "") exclude-dir))
        (inc (unless (string= include-dir "") include-dir)))
    (when (make-tags home
                     store
                     (lambda (f _)
                       (not (string-match "\\.tags$" f)))
                     (lambda (d a)
                       (cond ((string-match vcs d) nil)
                             ((and (stringp exc)
                                   (string-match exc d))
                              nil)
                             ((null inc) t)
                             (t (or (string-match inc d)
                                    (string-match inc a)))))
                     option
                     renew))))

;; end of `make-dir-tags'

;;; `make-dir-ctags'

(defun make-dir-ctags (dir tags options)
  "Make tags via ctags for specified DIR."
  (unless (string= "ctags" (*tags*))
    (user-error "%s" "ctags unavailable"))
  (let ((dir (path+ (expand-file-name dir)))
        (tags (expand-file-name tags))
        (options (concat "--options=" (expand-file-name options))))
    (let ((rc (shell-command* (*tags*)
                "-R"
                "-e"
                "-o" tags
                options
                dir)))
      (when (zerop (car rc))
        tags))))

;; end of `make-dir-ctags'


(provide 'tags)


;; end of tags.el
