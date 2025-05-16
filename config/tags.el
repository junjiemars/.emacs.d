;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;
;; features:
;;; 1. `mount-tags' and `unmount-tags'.
;;; 2. make tags for Directory, Emacs, Lisp, and C.
;;; 3. support `etags' and `ctags'.
;;;;


;;;
;; envrionment
;;;

(defun tags-spec->* (spec)
  "Return :tags of \\=`+nore-spec+\\='."
  (cond ((and spec (eq spec :root)) (emacs-home% ".tags/"))
        ((and spec (eq spec :nore)) (v-home% ".tags/nore.emacs.TAGS"))
        ((and spec (eq spec :emacs)) (v-home% ".tags/emacs.TAGS"))
        ((and spec (eq spec :prompt))
         (propertize "Make tags" 'face 'minibuffer-prompt))
        ((and spec (eq spec :out-dir))
         "^\\.\\|^out$\\|^bin$\\|^objs$\\|^[dD]ebug$\\|^[rR]elease$")
        ((and spec (eq spec :vcs-dir))
         ".git$\\|\\.hg$\\|\\.svn$")
        ((and spec (eq spec :arc-dir))
         "\\.bz2$\\|\\.gz$\\|\\.tgz$\\|\\.xz$\\|\\.Z$")
        ((and spec (eq spec :done)) "done")
        ((and spec (eq spec :failed)) "failed")))

(defun tags-program-check ()
  (or (let ((ctags (executable-find*
                    "ctags"
                    (lambda (bin)
                      (let ((ver (shell-command* bin "--version")))
                        (and (= 0 (car ver))
                             (string-match "^Exuberant Ctags" (cdr ver))))))))
        `( :bin ctags
           :cmd ,(concat ctags " -e %s -o %s -a %s")
           :opt ("--options=<file>"
                 "--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq"
                 "--langmap=c++:.h.cc--c++-kinds=+px")))
      (let ((etags (executable-find*
                    "etags"
                    (lambda (bin)
                      (let ((ver (shell-command* bin "--version")))
                        (and (= 0 (car ver))
                             (string-match "etags (GNU Emacs [.0-9]+)"
                                           (cdr ver))))))))
        `( :bin etags
           :cmd ,(concat etags " %s -o %s -a %s")
           :opt ("-l c" "-l lisp" "-l auto")))
      (let ((etags (concat (path- (car command-line-args)) "bin/etags")))
        (and (file-exists-p etags)
             `( :bin etags
                :cmd ,(concat etags " %s -o %s -a %s")
                :opt ("-l c" "-l lisp" "-l auto"))))))

(defalias '*tags*
  (let ((b (tags-program-check)))
    (lambda (&optional k v)
      (cond (k (plist-get b k))
            (v (setq b v))
            (t b))))
  "The tags program.\n
1st %s: tags options;
2nd %s: explicit name of file for tag table;
3rd %s: append to existing tag file.\n
\\=`tags-table-list\\=' should be persitent between sessions
when \\=`desktop-globals-to-save\\=' include it.")

(defvar *tags-option-history* nil
  "Tags option history list.")

(defun tags--read-option ()
  (read-string (concat (symbol-name (*tags* :bin)) " option: ")
               (car (or *tags-option-history*
                        (setq *tags-option-history* (*tags* :opt))))
               '*tags-option-history*))

(defun tags--unmount-prompt ()
  (list
   (cond (current-prefix-arg (yes-or-no-p "Unmount all? ") nil)
         ((consp tags-table-list)
          (read-file-name "Unmount from "
                          (file-name-directory (car tags-table-list))
                          tags-table-list
                          nil
                          (file-name-nondirectory (car tags-table-list))))
         (t (user-error "%s" "No mounted tags-table-list")))))

(defun tags--make-prompt ()
  (list (read-directory-name "Make tags for " source-directory)
        (tags--read-option)
        (y-or-n-p "Renew tags? ")))

(defun tags--dir-prompt ()
  (list (read-directory-name "Make tags for ")
        (read-file-name "Store tags in " nil nil nil ".tags")
        (when current-prefix-arg
          (read-string "Tags include regexp: " nil))
        (when current-prefix-arg
          (read-string "Tags exclude regexp: " nil))
        (tags--read-option)
        (y-or-n-p "Renew tags? ")))

;; end of environment


(defalias 'mount-tags #'visit-tags-table
  "Mount existing TAGS into \\=`tags-table-list\\='.")

(defun unmount-tags (&optional tags)
  "Unmount TAGS from \\=`tags-table-list\\='."
  (interactive (tags--unmount-prompt))
  (setq tags-file-name nil
        tags-table-list
        (when tags
          (let ((fn (expand-file-name tags))
                (xs nil))
            (dolist (x tags-table-list (nreverse xs))
              (unless (string-equal x fn)
                (setq xs (cons x xs))))))))

(defun dir-iterate (dir ff &optional df fp dp env)
  "Iterate DIR.\n
FF file-filter (lambda (filename absolute-name env)...),
DF dir-filter (lambda (dirname absolute-name env)...),
FP file-processor (lambda (filename-absolute env)...),
DP dir-processor (lambda (dirname-absolute env)...),
ENV (:k1 v1 :k2 v2 ...)."
  (inhibit-file-name-handler
    (let ((files (directory-files dir t)))
      (while files
        (let* ((a (car files))
               (s (file-attributes a))
               (ft (nth 0 s))
               (f (file-name-nondirectory a))
               (len (length f)))
          (cond ((eq ft t) (cond ((char-equal (aref f (1- len)) ?.) nil)
                                 ((and df (funcall df f a env))
                                  (and dp (funcall dp a env))
                                  (dir-iterate a ff df fp dp env))))
                ((stringp ft) nil)
                ((null ft) (and ff (funcall ff f a env)
                                fp (funcall fp a env))))
          (setq files (cdr files)))))))

(defun tags--file-processor (file &optional env)
  (let* ((env-file (plist-get env :file))
         (env-option (plist-get env :option))
         (env-cmd (plist-get env :cmd))
         (quoted (shell-quote-argument file))
         (cmd (format env-cmd (or env-option "") env-file quoted))
         (rc (shell-command* cmd))
         (done (= 0 (car rc))))
    (message "%s %s...%s"
             (tags-spec->* :prompt) cmd
             (if done (tags-spec->* :done) (tags-spec->* :failed)))
    (and done file)))

(defun make-tags
    (home tags-file file-filter dir-filter &optional tags-option renew env)
  "Make tags.\n
HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
TAGS-OPTION \\=`*tags*\\=' extra options,
RENEW overwrite the existing tags file when t else create it."
  (unless (*tags* :bin)
    (error "%s" "No tags program found"))
  (when (file-exists-p home)
    (let* ((tf (expand-file-name tags-file))
           (td (file-name-directory tf))
           (prompt (tags-spec->* :prompt)))
      (cond ((file-exists-p tf) (and renew (delete-file tf)))
            (t (path! td)))
      (dir-iterate home
                   file-filter
                   dir-filter
                   #'tags--file-processor
                   nil
                   (append (list :file tf
                                 :option tags-option
                                 :prompt prompt
                                 :cmd (*tags* :cmd))
                           env))
      (message "%s for %s...%s"
               prompt tf
               (if (file-exists-p tf)
                   (tags-spec->* :done)
                 (tags-spec->* :failed))))))

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

;; end of file/dir filters

;;; make tags

(defun make-c-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for C source code in HOME."
  (make-tags home
             tags-file
             (or file-filter #'tags--file-filter)
             (or dir-filter #'tags--dir-filter)
             option
             renew
             `( :inc "\\.[ch]+$"
                :exc ,(concat (tags-spec->* :vcs-dir)
                              "\\|" (tags-spec->* :arc-dir)
                              "\\|" (tags-spec->* :out-dir)))))

(defun make-lisp-tags
    (home tags-file &optional option file-filter dir-filter renew)
  "Make TAGS-FILE for Lisp source code in HOME."
  (make-tags home
             tags-file
             (or file-filter #'tags--file-filter)
             (or dir-filter #'tags--dir-filter)
             option
             renew
             `( :inc "\\.el$\\|\\.cl$\\|\\.lis[p]?$\\|\\.ss$\\|\\.scm$"
                :exc ,(concat (tags-spec->* :vcs-dir)
                              "\\|" (tags-spec->* :arc-dir)))))

(defun make-emacs-tags (source &optional option renew)
  "Make tags for Emacs\\=' C and Lisp SOURCE code."
  (interactive (tags--make-prompt))
  (let ((file (tags-spec->* :emacs)))
    (make-c-tags (concat source "src/") file option
                 nil nil renew)
    (make-lisp-tags (concat source "lisp/") file option)))

(defun make-dir-tags (dir store &optional include exclude option renew)
  "Make tags for specified DIR."
  (interactive (tags--dir-prompt))
  (let ((home (path+ (expand-file-name dir) "/"))
        (exc (concat (tags-spec->* :out-dir)
                     "\\|" (tags-spec->* :vcs-dir)
                     "\\|" (tags-spec->* :arc-dir)
                     (unless (string-equal exclude "") "\\|" exclude)))
        (inc (unless (string-equal include "") "\\|" include)))
    (make-tags home
               store
               #'tags--file-filter
               #'tags--dir-filter
               option
               renew
               (list :exc exc :inc inc))))

(defun make-dir-ctags (dir tags options)
  "Make tags via ctags for specified DIR."
  (let ((tp (symbol-name (*tags* :bin))))
    (unless (string-equal "ctags" tp)
      (error "%s" "No ctags program found"))
    (let ((d1 (path+ (expand-file-name dir) "/"))
          (f1 (expand-file-name tags))
          (o1 (concat "--options=" (expand-file-name options)))
          (prompt (tags-spec->* :prompt)))
      (message "%s %s %s %s ..." prompt tp o1 dir )
      (let* ((rc (shell-command* tp "-e" o1 "-o" f1 "-R"d1))
             (done (= 0 (car rc))))
        (message "%s for %s...%s"
                 prompt dir
                 (if done
                     (tags-spec->* :done)
                   (tags-spec->* :failed)))
        (and done f1)))))

;; end of make tags


(provide 'tags)

;; end of tags.el
