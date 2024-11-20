;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;
;; Commentary: tagging
;;;;

(defalias '*tags*
  (lexical-let%
      ((b (or (executable-find%
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
    (lambda (&optional n)
      (plist-get b (or n :bin))))
  "The tags program.\n
1st %s: tags options;
2nd %s: explicit name of file for tag table;
3rd %s: append to existing tag file.\n
\\=`tags-table-list\\=' should be persitent between sessions
when \\=`desktop-globals-to-save\\=' include it.")

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
                       "%s" "No mounted tags-table-list"))))))
  (setq tags-file-name nil
        tags-table-list
        (when tags
          (let ((fn (expand-file-name tags))
                (xs nil))
            (dolist* (x tags-table-list (nreverse xs))
              (unless (string= x fn)
                (setq xs (cons x xs))))))))

(defun dir-iterate (dir ff df fn dn)
  "Iterate DIR.\n
FF file-filter (lambda (file-name absolute-name)...),
DF dir-filter (lambda (dir-name absolute-name)...),
FN file-processor (lambda (absolute-name)...),
DN dir-processor (lambda (aboslute-name)...)."
  (inhibit-file-name-handler
    (let ((files (directory-files dir t)))
      (while files
        (let* ((a (car files))
               (s (file-attributes a)) (ft (nth 0 s))
               (f (file-name-nondirectory a))
               (len (length f)))
          (cond ((eq ft t) (cond ((char= (aref f (1- len)) ?.))
                                 ((and df (funcall df f a))
                                  (and dn (funcall dn a))
                                  (dir-iterate a ff df fn dn))))
                ((stringp ft))
                (ff (cond ((funcall ff f a) (and fn (funcall fn a))))))
          (setq files (cdr files)))))))

(defun dir-backtrack (dir prefer)
  "Backtrack DIR.\n
Starting at DIR, look up directory hierarchy for prefered
directory or file. Ignore the symbol links of directory.\n
PREFER (lambda (dir files)...)."
  (inhibit-file-name-handler
    (let ((stop "\\(^/\\|[a-zA-Z]:/\\)\\'"))
      (while (and (directory-name-p dir) (null (string-match stop dir)))
        (and prefer
             (funcall prefer dir
                      (let ((xs nil))
                        (dolist* (x (directory-files dir t) xs)
                          (let ((f (file-name-nondirectory x))
                                (ft (nth 0 (file-attributes x))))
                            (unless (or (and (eq ft t)
                                             (or (string= "." f)
                                                 (string= ".." f)))
                                        (stringp ft))
                              (setq xs (cons f xs))))))))
        (setq dir (path- dir))))))

(defun make-tags
    (home tags-file file-filter dir-filter &optional tags-option renew)
  "Make tags.\n
HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
TAGS-OPTION \\=`*tags*\\=' extra options,
RENEW overwrite the existing tags file when t else create it."
  (unless (*tags*)
    (error "%s" "No tags program found"))
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
    (error "%s" "No ctags program found"))
  (let ((dir (path+ (expand-file-name dir)))
        (tags (expand-file-name tags))
        (options (concat "--options=" (expand-file-name options))))
    (let ((rc (shell-command* (*tags*)
                "-R"
                "-e"
                "-o" tags
                options
                dir)))
      (and (zerop (car rc)) tags))))

;; end of `make-dir-ctags'


(provide 'tags)


;; end of tags.el
