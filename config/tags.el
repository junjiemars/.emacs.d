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
  (cond ((executable-find%
          "ctags"
          (lambda (bin)
            (let ((ver (shell-command* bin "--version")))
              (and (zerop (car ver))
                   (string-match "Exuberant Ctags [.0-9]+"
                                 (cdr ver))))))
         "ctags -e -o %s -a %s ; echo %s")
        ((executable-find%
          "etags"
          (lambda (bin)
            (let ((ver (shell-command* bin "--version")))
              (and (zerop (car ver))
                   (string-match "Exuberant Ctags [.0-9]+"
                                 (cdr ver))))))
         ;; on Linux/Darwin ctags may be has the synonym: etags
         "etags -e -o %s -a %s ; echo %s")
        ((executable-find%
          "etags"
          (lambda (bin)
            (let ((ver (shell-command* bin "--version")))
              (and (zerop (car ver))
                   (string-match "etags (GNU Emacs [.0-9]+)"
                                 (cdr ver))))))
         "etags -o %s -l auto -a %s ; echo %s"))
  "The default tags program.
This is used by commands like `make-tags'.

The default is \"ctags -e -o %s -a %s ; echo %s\", 
first %s: explicit name of file for tag table; overrides default TAGS or tags.
second %s: append to existing tag file.
third %s: echo source file name in *Messages* buffer.

`tags-table-list' should be persitent between sessions 
when `desktop-globals-to-save' include it."
  :type 'string
  :group 'tags)


(defcustom% tags-in-view-mode
  `(list source-directory
         (when% (> (path-depth (expand-file-name
                                (path- invocation-directory)))
                   2)
           (path- invocation-directory))
         (when-var% package-user-dir 'package
           package-user-dir)
         (v-home* "config/")
         (v-home* "private/")
         (v-home* "theme/"))
  "The `current-buffer' should open in `view-mode'."
  :type 'list
  :group 'tags)


(defun mount-tags (tags &optional arg)
  "Mount existing TAGS into `tags-table-list'.
With prefix ARG decide to append the end of `tags-table-list' or not."
  (interactive "fmount tags from: \nP")
  (add-to-list 'tags-table-list
               (expand-file-name tags)
               arg
               #'string=))


(defun unmount-tags (tags)
  "Unmount TAGS from `tags-table-list'."
  (interactive "Funmount tags from: ")
  (setq tags-table-list
        (remove** (expand-file-name tags)
                  tags-table-list :test #'string=)))


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
                     (message
                      "make-tags: %s ... %s"
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
    (make-tags (emacs-home*) tags-file lisp-ff home-df renew)))


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


(defun make-dir-tags (dir &optional renew)
  "Make and mount tags for specified DIR."
  (interactive "Dmake tags in \nP")
  (when (file-exists-p dir)
    (let* ((home (path+ (expand-file-name dir)))
           (tags-file (concat home ".tags")))
      (when (make-tags home
                       tags-file
                       (lambda (f _)
                         (not (string-match
                               "\\\.tags$" f)))
                       (lambda (d _)
                         (not (string-match
                               "^\\\.git/$\\|^out/$\\|^build/$" d)))
                       renew)
        (mount-tags tags-file)))))


;; go into `view-mode'

;; `find-tag' or `xref-find-definitions' into `view-mode'
(if-fn% 'xref-find-definitions 'xref
        (progn
          ;; `xref-find-definitions' into `view-mode'
          (defadvice xref-find-definitions
              (after xref-find-definitions-after disable)
            (with-current-buffer (current-buffer)
              (when (file-in-dirs-p (buffer-file-name* (current-buffer))
                                    tags-in-view-mode)
                (view-mode 1))))
          
          (with-eval-after-load 'xref
            (ad-enable-advice #'xref-find-definitions 'after
                              "xref-find-definitions-after")
            (ad-activate #'xref-find-definitions t)))
  
  ;; pop-tag-mark same as Emacs22+ for ancient Emacs
  (when-fn% 'pop-tag-mark 'etags
    (with-eval-after-load 'etags
      ;; define keys for `pop-tag-mark' and `tags-loop-continue'
      (define-key% (current-global-map) (kbd "M-,") #'pop-tag-mark)
      (define-key% (current-global-map) (kbd "M-*") #'tags-loop-continue)))

  ;; find-tag into `view-mode'
  (defadvice find-tag (after find-tag-after disable)
    (with-current-buffer (current-buffer)
      (when (file-in-dirs-p (buffer-file-name* (current-buffer))
                            tags-in-view-mode)
        (view-mode 1))))
  
  (with-eval-after-load 'etags
    (ad-enable-advice #'find-tag 'after "find-tag-after")
    (ad-activate #'find-tag t)))


;; open emacs source in `view-mode'
(with-eval-after-load 'help-mode

  (lexical-let% ((help-fn (button-type-get 'help-function-def 'help-function)))
    (button-type-put
     'help-function-def 'help-function
     #'(lambda (fn &optional file)
         (funcall help-fn fn file)
         (view-mode 1))))

  (lexical-let% ((help-fn (button-type-get 'help-variable-def 'help-function)))
    (button-type-put
     'help-variable-def 'help-function
     #'(lambda (var &optional file)
         (funcall help-fn var file)
         (view-mode 1)))))

 ;; end of go into `view-mode'


(provide 'tags)
