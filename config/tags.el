;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; tags.el
;;;;


(defmacro tags-spec->% (&rest key)
  "Extract value from the list of spec via KEYS at compile time.

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


(defcustom% tags-program
  (cond ((executable-find% "ctags"
                           (lambda (bin)
                             (let ((ver (shell-command* bin "--version")))
                               (when (zerop (car ver))
                                 (string-match "Exuberant Ctags [.0-9]+"
                                               (cdr ver))))))
         "ctags -e -o %s -a %s ; echo %s")
        ((executable-find% "etags"
                           (lambda (bin)
                             (let ((ver (shell-command* bin "--version")))
                               (when (zerop (car ver))
                                 (string-match "Exuberant Ctags [.0-9]+"
                                               (cdr ver))))))
         ;; on Linux/Darwin ctags may be has the synonym: etags
         "etags -e -o %s -a %s ; echo %s")
        ((executable-find% "etags"
                           (lambda (bin)
                             (let ((ver (shell-command* bin "--version")))
                               (when (zerop (car ver))
                                 (string-match "etags (GNU Emacs [.0-9]+)"
                                               (cdr ver))))))
         "etags -o %s -l auto -a %s ; echo %s"))
  "The default tags program.
This is used by commands like `make-tags' and others.

The default is \"ctags -e -o %s -a %s ; echo %s\", 
first %s: explicit name of file for tag table; overrides default TAGS or tags.
second %s: append to existing tag file.
third %s: echo source file name in *Messages* buffer.

`tags-table-list' should be persitent between sessions 
when `desktop-globals-to-save' include it."
  :type 'string
  :group 'tags)


(defun mount-tags (&rest tags-file)
  "Mount existing TAGS-FILE into `tags-table-list'."
  (declare (indent 0))
  (let ((mounted nil))
    (dolist (x tags-file (nreverse mounted))
      (when (file-exists-p x)
        (add-to-list 'tags-table-list x t #'string=)
        (setq mounted (cons x mounted))))))

(defun unmount-tags (tags-file)
  "Unmount TAGS-FILE from `tags-table-list'."
  (when tags-file
    (setq tags-table-list
          (remove-if (lambda (x) (string= x tags-file))
                     tags-table-list))))


(defun make-tags (home tags-file file-filter dir-filter &optional renew)
  "Make tags.

HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
RENEW overwrite the existing tags file when t else create it.
"
  (unless tags-program
    (signal 'void-variable (list 'tags-program tags-program)))
  (when (file-exists-p home)
    (let ((tags-dir (file-name-directory tags-file)))
      (if (file-exists-p tags-file)
          (when renew (delete-file tags-file))
        (when (not (file-exists-p tags-dir))
          (make-directory tags-dir t)))
      (dir-iterate home
                   file-filter
                   dir-filter
                   (lambda (f)
                     (message "make-tags: %s ... %s"
                              f
                              (if (zerop
                                   (car (shell-command*
                                            (format tags-program tags-file
                                                    (shell-quote-argument f)
                                                    (shell-quote-argument f)))))
                                  "ok" "failed")))
                   nil)
      (when (file-exists-p tags-file)
        (add-to-list 'tags-table-list tags-file t #'string=)
        tags-file))))


(defun make-emacs-home-tags (tags-file &optional renew)
  "Make TAGS-FILE for Emacs' home directory.

Example:
 (make-emacs-home-tags (tags-spec->% :emacs-home) t)
"
  (let ((lisp-ff (lambda (f _) (string-match "\\\.el$" f)))
        (home-df
         (lambda (d _)
           (not
            (string-match
             "^\\\..*/$\\|^theme/$\\|^g_.*/$\\|^t_.*/$\\|^private/$" d)))))
    (make-tags +emacs-home+ tags-file lisp-ff home-df renew)))


(defun make-emacs-source-tags (tags-file src-root &optional renew)
  "Make TAGS-FILE for Emacs' C and Lisp source code in SRC-ROOT.

Example:
 (make-emacs-source-tags
   (`tags-spec->%' :emacs-home)
   `source-directory'
    t)
"
  (let ((lisp-ff (lambda (f _) (string-match "\\\.el$" f)))
        (c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (lambda (_ __) t)))
    (when (file-exists-p (concat src-root "src/"))
      (make-tags (concat src-root "src/") tags-file c-ff df renew))
    (when (file-exists-p (concat src-root "lisp/"))
      (make-tags (concat src-root "lisp/") tags-file lisp-ff df))))


(defun make-c-tags (home tags-file &optional dir-filter renew)
  "Make TAGS-FILE for C source code in HOME."
  (let ((c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (or dir-filter
                (lambda (d _)
                  (not
                   (string-match
                    "^\\\.git/$\\|^out/$\\|^objs/$\\|^c\\\+\\\+/$"
                    d))))))
    (make-tags home tags-file c-ff df renew)))


(defun make-system-c-tags (includes &optional dir-filter renew)
  "Make tags for system INCLUDES.

INCLUDES should be the system C include directories list,
DIR-FILTER directory filter function,
RENEW overwrite the existing tags file when t else create it.

Example:
 (make-system-c-tags (`system-cc-include' t) nil t)
"
  (let ((tag-file (tags-spec->% :os-include)))
    (make-c-tags (car includes) tag-file dir-filter renew)
    (dolist (p (cdr includes) tag-file)
      (make-c-tags p tag-file dir-filter))))


(defun make-dir-tags (dir &optional renew)
  "Make and mount tags for specified DIR."
  (interactive "D make tags in ")
  (when (file-exists-p dir)
    (let* ((home (path+ (expand-file-name dir)))
           (tags-file (concat home ".tags")))
      (when (make-tags home
                       tags-file
                       (lambda (f _)
                         (not (string-match "\\\.tags$" f)))
                       (lambda (d _)
                         (not (string-match "^\\\.git/$\\|^out/$\\|^build/$" d)))
                       renew)
        (mount-tags tags-file)))))
  

(provide 'tags)
