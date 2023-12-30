;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;
;; Commentary: tagging
;;;;


(defmacro tags-spec->% (&rest key)
  "Extract value from the list of spec via KEYS at compile time."
  `(self-spec->% (list
                  :emacs-home ,(v-home% ".tags/home/TAGS")
                  :emacs-source ,(v-home% ".tags/source/TAGS")
                  :os-include ,(emacs-home* ".tags/os/TAGS"))
                 ,@key))


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
                (list (eval-when-compile
                        (inhibit-file-name-handler
                          (when% (> (path-depth
                                     (expand-file-name
                                      (path- invocation-directory)))
                                    2)
                            (path- invocation-directory))))
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
         `("--langmap=c:.h.c --c-kinds=+px"
           "--langmap=c++:.h.cc--c++-kinds=+px"
           ,(eval-when-compile
              (let ((rc (shell-command* "rustc" "--print sysroot")))
                (when (zerop (car rc))
                  (let* ((s "/lib/rustlib/src/rust/src/etc/ctags.rust")
                         (c (concat (string-trim> (cdr rc)) s)))
                    (when (file-exists-p c)
                      (concat "--options=" c))))))))
        ((string= "etags" (*tags*))
         `("-l c" "-l lisp" "-l auto"))
        (t nil))
  "Tags option history list.")

(defvar *tags-skip-history*
  (list "cpp\\|c\\+\\+\\|/python.*?/\\|/php.*?/\\|/ruby.*?/\\|/swift/")
  "Tags skip history list.")


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
                      (user-error
                       "%s" "tags-table-list alreay empty"))))))
  (setq tags-file-name nil
        tags-table-list
        (when tags
          (let ((fn (expand-file-name tags)))
            (remove-if* (lambda (x) (string= x fn))
                        tags-table-list)))))


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
      (let ((header (propertize "make-tags"
                                'face 'minibuffer-prompt)))
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
             (message "%s %s... %s" header cmd
                      (if (zerop (car (shell-command* cmd)))
                          "ok"
                        "failed"))))
         nil)
        (message "%s for %s ... %s" header tf (if (file-exists-p tf)
                                                  "done"
                                                "failed"))))))


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
                     "^\\.git/$\\|\\.svn/$\\|^out/$\\|^objs/$"
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
                     "^\\.git/$\\|^\\.svn/$\\|^out/$\\|^objs/$"
                     d))))
             option
             renew))


(defun make-emacs-home-tags (&optional option renew)
  "Make tags for \\=`emacs-home*\\=' directory."
  (interactive (list
                (read-string (concat (*tags*) " option: ")
                             (car *tags-option-history*)
                             '*tags-option-history*)
                (y-or-n-p "tags renew? ")))
  (make-lisp-tags (emacs-home*)
                  (tags-spec->% :emacs-home)
                  option
                  (lambda (f _)
                    (string-match "\\.el$" f))
                  (lambda (d _)
                    (string-match "^config/$" d))
                  renew))


(defun make-emacs-source-tags (source &optional option renew)
  "Make tags for Emacs' C and Lisp SOURCE code."
  (interactive (list (read-directory-name "make tags for " source-directory)
                     (read-string (concat (*tags*) " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (make-c-tags (concat source "src/")
               (tags-spec->% :emacs-source)
               option
               (lambda (f _) (string-match "\\.[ch]$" f))
               (lambda (_ __) t)
               renew)
  (make-lisp-tags (concat source "lisp/")
                  (tags-spec->% :emacs-source)
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
        (vcs "^\\.git/$\\|\\.svn/$")
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


(provide 'tags)


;; end of tags.el
