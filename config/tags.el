;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;
;; Commentary: tagging
;;;;



;;;
;; tags envrionment
;;;

(defun tags*-check ()
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
                        ,(concat ,bin " %s -o %s -a %s"))))))))

(defalias '*tags*
  (lexical-let% ((b (tags*-check)))
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

(defvar +tags-make-prompt+
  (propertize "Make tags" 'face 'minibuffer-prompt))

(defvar *tags-out-dir*
  "^\\.\\|^out$\\|^bin$\\|^objs$\\|^[dD]ebug$\\|^[rR]elease$")

(defvar *tags-vcs-dir*
  ".git$\\|\\.hg$\\|\\.svn$")

(defvar *tags-archive-dir*
  "\\.bz2$\\|\\.gz$\\|\\.tgz$\\|\\.xz$\\|\\.Z$")

;; end of tags environment

(defalias 'mount-tags #'visit-tags-table
  "Mount existing TAGS into \\=`tags-table-list\\='.")

(defun unmount-tags (&optional tags)
  "Unmount TAGS from \\=`tags-table-list\\='."
  (interactive (list
                (unless (and current-prefix-arg
                             (yes-or-no-p "unmount all? "))
                  (fluid-let (file-name-history tags-table-list)
                    (if (consp file-name-history)
                        (read-file-name "unmount from ")
                      (user-error "%s" "No mounted tags-table-list"))))))
  (setq tags-file-name nil
        tags-table-list
        (when tags
          (let ((fn (expand-file-name tags))
                (xs nil))
            (dolist* (x tags-table-list (nreverse xs))
              (unless (string= x fn)
                (setq xs (cons x xs))))))))

(defun dir-iterate (dir ff &optional df fp dp env)
  "Iterate DIR.\n
FF file-filter (lambda (filename absolute-name env)...),
DF dir-filter (lambda (dirname absolute-name env)...),
FP file-processor (lambda (filename-absolute env)...),
DP dir-processor (lambda (dirname-absolute env)...),
ENV (:k1 v1 :k2 v2 ...)."
  ;; (inhibit-file-name-handler)
  (let ((files (directory-files dir t)))
    (while files
      (let* ((a (car files))
             (s (file-attributes a)) (ft (nth 0 s))
             (f (file-name-nondirectory a))
             (len (length f)))
        (cond ((eq ft t) (cond ((char= (aref f (1- len)) ?.) nil)
                               ((and df (funcall df f a env))
                                (and dp (funcall dp a env))
                                (dir-iterate a ff df fp dp env))))
              ((stringp ft) nil)
              ((null ft) (and ff (funcall ff f a env)
                              fp (funcall fp a env))))
        (setq files (cdr files))))))

(defun tags-file-processor (f &optional env)
  (let ((tf (plist-get env :tags-file))
        (opt (plist-get env :tags-option))
        (fmt (plist-get env :tags-cmd))
        (qf (shell-quote-argument f)))
    (let* ((cmd (format fmt (or opt "") tf qf))
           (rc (shell-command* cmd))
           (done (zerop (car rc))))
      (message "%s %s ... %s"
               +tags-make-prompt+ cmd (if done "ok" "failed"))
      (and done f))))

(defun make-tags (home tags-file file-filter dir-filter
                       &optional tags-option renew env)
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
      (cond ((file-exists-p tf) (when renew (delete-file tf)))
            (t (path! td)))
      (let ((env (append (list :tags-file tf
                               :tags-option tags-option
                               :tags-prompt +tags-make-prompt+
                               :tags-cmd (*tags* :cmd))
                         env)))
        (dir-iterate home
                     file-filter
                     dir-filter
                     #'tags-file-processor
                     nil
                     env)
        (message "%s for %s ... %s"
                 +tags-make-prompt+ tf
                 (if (file-exists-p tf) "done" "failed"))))))

;;; file/dir filters

(defun tags--file-filter (fd &optional _ env)
  (let ((exc (plist-get env :exc))
        (inc (plist-get env :inc)))
    (cond ((and exc (string-match exc fd)) nil)
          ((null inc) t)
          ((and inc (string-match inc fd)) t))))

(defun tags--dir-filter (fd &optional _ env)
  (let ((exc (plist-get env :exc))
        (inc (plist-get env :inc)))
    (cond ((and exc (string-match exc fd)) nil)
          ((null inc) t)
          (inc (and (string-match inc fd) t))
          (t nil))))

(defun tags-c-file-filter (f &rest _)
  (declare (pure t))
  (string-match "\\.[ch]+$" f))

(defun tags-c-dir-filter (d &rest _)
  (declare (pure t))
  (null (string-match (concat *tags-vcs-dir*
                              "\\|" *tags-archive-dir*
                              "\\|" *tags-out-dir*)
                      d)))

(defun tags-lisp-file-filter (f &rest _)
  (declare (pure t))
  (string-match "\\.el$\\|\\.cl$\\|\\.lis[p]?$\\|\\.ss$\\|\\.scm$" f))

(defun tags-lisp-dir-filter (d &rest _)
  (declare (pure t))
  (null (string-match (concat *tags-vcs-dir*
                              "\\|" *tags-archive-dir*)
                      d)))

;; end of file/dir filters

;;; make tags

(defun make-c-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for C source code in HOME."
  (make-tags home
             tags-file
             (or file-filter #'tags-c-file-filter)
             (or dir-filter #'tags-c-dir-filter)
             option
             renew))

(defun make-lisp-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for Lisp source code in HOME."
  (make-tags home
             tags-file
             (or file-filter #'tags-lisp-file-filter)
             (or dir-filter #'tags-lisp-dir-filter)
             option
             renew))

(defun make-nore-tags (&optional option renew)
  "Make tags for Nore \\=`emacs-home%\\=' directory."
  (interactive (list (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (make-lisp-tags (emacs-home%)
                  (tags-spec->% :nore)
                  option
                  #'tags-lisp-file-filter
                  (lambda (d _)
                    (string= "config" d))
                  renew))

(defun make-emacs-tags (source &optional option renew)
  "Make tags for Emacs\\=' C and Lisp SOURCE code."
  (interactive (list (read-directory-name "make tags for " source-directory)
                     (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (make-c-tags (concat source "src/")
               (tags-spec->% :emacs)
               option
               #'tags-c-file-filter
               #'true
               renew)
  (make-lisp-tags (concat source "lisp/")
                  (tags-spec->% :emacs)
                  option
                  #'tags-lisp-file-filter
                  #'true))

(defun make-dir-tags (dir store &optional include exclude option renew)
  "Make tags for specified DIR."
  (interactive (list (read-directory-name "make tags for ")
                     (read-file-name "store tags in " nil nil nil ".tags")
                     (when current-prefix-arg
                       (read-string "tags include regexp: " nil))
                     (when current-prefix-arg
                       (read-string "tags exclude regexp: " nil))
                     (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (let ((home (path+ (expand-file-name dir)))
        (exc (concat *tags-out-dir*
                     "\\|" *tags-vcs-dir*
                     "\\|" *tags-archive-dir*
                     (unless (string= exclude "") "\\|" exclude)))
        (inc (unless (string= include "") "\\|" include)))
    (when (make-tags home
                     store
                     #'tags--file-filter
                     #'tags--dir-filter
                     option
                     renew
                     (list :exc exc :inc inc)))))

(defun make-dir-ctags (dir tags options)
  "Make tags via ctags for specified DIR."
  (let ((tp (*tags*)))
    (unless (string= "ctags" tp)
      (error "%s" "No ctags program found"))
    (let ((d1 (path+ (expand-file-name dir)))
          (f1 (expand-file-name tags))
          (o1 (concat "--options=" (expand-file-name options))))
      (message "%s %s %s %s ..." +tags-make-prompt+ dir tp o1)
      (let* ((rc (shell-command* tp "-R" "-e" "-o" f1 o1 d1))
             (done (zerop (car rc))))
        (message "%s for %s ... %s"
                 +tags-make-prompt+ dir (if done "done" "failed"))
        (and done f1)))))

;; end of make tags


(provide 'tags)

;; end of tags.el
