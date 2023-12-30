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
  "Extract value from the list of spec via KEYS at compile time.\n
Examples:
 (tags-spec->% :emacs-home)
 (tags-spec->% :emacs-source)
 (tags-spec->% :os-include)
"
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
 "The default tags program.
This is used by commands like \\=`make-tags\\='.\n
The default is \"ctags -e %s -o %s -a %s\",
first %s: ctags options;
second %s: explicit name of file for tag table;
third %s: append to existing tag file.\n
\\=`tags-table-list\\=' should be persitent between sessions
when \\=`desktop-globals-to-save\\=' include it."
 :type '(plist :key-type 'symbol :value-type 'string)
 :group 'tags)


(defalias '*tags*
  (lexical-let% ((b tags-program))
    (lambda (&optional n)
  		"N"
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
  		"N"
      (cond (n (setq b (cons n b)))
            (t b))))
  "Tag's buffer should open in \\=`view-mode\\='.")


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
  "Unmount TAGS from \\=`tags-table-list\\='.\n
With prefix argument TAGS unmount all tags from
\\=`tags-table-list\\='."
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
  "Make tags for Emacs' home directory."
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


;;; `xref-find-definitions' macro

(defmacro-if-fn% xref-find-definitions xref)

(defmacro when-fn-xref-find-definitions% (&rest body)
  `(if-fn-xref-find-definitions%
       (progn% ,@body)))

(defmacro unless-fn-xref-find-definitions% (&rest body)
  `(if-fn-xref-find-definitions%
       (comment ,@body)
     (progn% ,@body)))

;; end of `xref-find-definitions' macro

;;;
;; go into `view-mode'
;; `xref-find-definitions' into `view-mode'
;;;

(when-fn-xref-find-definitions%
 ;; `xref-find-definitions' into `view-mode'
 (defadvice xref-find-definitions
     (after xref-find-definitions-after disable)
   (with-current-buffer (current-buffer)
     (when (file-in-dirs-p (buffer-file-name (current-buffer))
                           (tags-in-view-mode))
       (view-mode 1)))))

(when-fn-xref-find-definitions%
 (defun on-xref-init! ()
   (ad-enable-advice #'xref-find-definitions 'after
                     "xref-find-definitions-after")
   (ad-activate #'xref-find-definitions t)))


(unless-fn% 'xref-find-references 'xref
  (defun xref-find-references (what)
    "Alias of \\=`tags-apropos\\='."
    (interactive
     (list (read-string "Find references of: "
                        (cdr (symbol@ 'symbol)))))
    (tags-apropos what)))


;; end of `xref'

;;;
;; `etags' after load
;; `pop-tag-mark' same as Emacs22+ for ancient Emacs
;;;

(defmacro when-feature-etags% (&rest body)
  `(unless-fn-xref-find-definitions% ,@body))

(defmacro when-fn-pop-tag-mark% (&rest body)
  `(unless-fn-xref-find-definitions% ,@body))

;;; `find-tag' into `view-mode'
(unless-fn-xref-find-definitions%
 (defadvice find-tag (after find-tag-after disable)
   (with-current-buffer (current-buffer)
     (when (file-in-dirs-p (buffer-file-name (current-buffer))
                           (tags-in-view-mode))
       (view-mode 1)))))

(defun on-etags-init! ()
  "On \\=`etags\\=' initialization."
  (when-fn-pop-tag-mark%
   ;; define keys for `pop-tag-mark' and `tags-loop-continue'
   (define-key% (current-global-map) (kbd "M-,") #'pop-tag-mark)
   (define-key% (current-global-map) (kbd "M-*") #'tags-loop-continue))
  (unless-fn-xref-find-definitions%
   (ad-enable-advice #'find-tag 'after "find-tag-after")
   (ad-activate #'find-tag t)))

;; end of `etags'

;;;
;; after-load
;;;

;;; `xref' after load
(when-fn-xref-find-definitions%
 (with-eval-after-load 'xref
   (on-xref-init!)))

;;; `etags' after load
(when-feature-etags%
 (with-eval-after-load 'etags
   (on-etags-init!)))

;; end of after-load


(provide 'tags)


;; end of tags.el
