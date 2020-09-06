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
         "ctags -e %s -o %s -a %s")
        ((executable-find%
          "etags"
          (lambda (bin)
            (let ((ver (shell-command* bin "--version")))
              (and (zerop (car ver))
                   (string-match "Exuberant Ctags [.0-9]+"
                                 (cdr ver))))))
         ;; on Linux/Darwin ctags may be has the synonym: etags
         "etags -e %s -o %s -a %s")
        ((executable-find%
          "etags"
          (lambda (bin)
            (let ((ver (shell-command* bin "--version")))
              (and (zerop (car ver))
                   (string-match "etags (GNU Emacs [.0-9]+)"
                                 (cdr ver))))))
         "etags %s -o %s -a %s"))
  "The default tags program.
This is used by commands like `make-tags'.

The default is \"ctags -e %s -o %s -a %s\", 
first %s: ctags options
second %s: explicit name of file for tag table; overrides default TAGS or tags.
third %s: append to existing tag file.

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


(defvar *tags-option-history*
  (let ((bin (string-trim> tags-program " +.*")))
    (cond ((string= "etags" bin) (list "-l c" "-l auto"))
          ((string= "ctags" bin) (list "--c-kinds=+p"))
          (t "")))
  "Tags option history list.")

(defvar *tags-skip-history*
  (list "Frameworks.*$\\|cpp.*$\\|c\\+\\+.*$\\|php.*$\\|ruby.*$")
  "Tags option history list.")


(defun mount-tags (tags &optional append)
  "Mount existing TAGS into `tags-table-list'.

With prefix argument APPEND TAGS to the tail of `tags-table-list'."
  (interactive "fmount tags from \nP")
  (add-to-list 'tags-table-list
               (expand-file-name tags)
               append
               #'string=))


(defun unmount-tags (&optional tags)
  "Unmount TAGS from `tags-table-list'.

With prefix argument TAGS unmount all tags from `tags-table-list'."
  (interactive (list (when (not current-prefix-arg)
                       (read-file-name "unmount tags from "))))
  (setq tags-table-list
        (if tags
            (remove** (expand-file-name tags)
                      tags-table-list :test #'string=)
          nil)))


(defun make-tags (home tags-file file-filter dir-filter
                       &optional tags-option renew)
  "Make tags.

HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
TAGS-OPTION `tags-program' extra options,
RENEW overwrite the existing tags file when t else create it."
  (unless tags-program
    (signal 'void-variable (list 'tags-program tags-program)))
  (when (file-exists-p home)
    (let* ((tf (expand-file-name tags-file))
           (td (file-name-directory tf)))
      (if (file-exists-p tf)
          (when renew (delete-file tf))
        (unless (file-exists-p td)
          (make-directory td t)))
      (let ((header (propertize "make-tags"
                                'face 'minibuffer-prompt)))
        (dir-iterate home
                     file-filter
                     dir-filter
                     (lambda (f)
                       (let ((cmd (format tags-program
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


(defun make-emacs-home-tags (&optional tags-file option)
  "Make TAGS-FILE for Emacs' home directory.

Example:
 (make-emacs-home-tags (tags-spec->% :emacs-home))
"
  (interactive)
  (make-tags (emacs-home*)
             (or tags-file
                 (tags-spec->% :emacs-home))
             (lambda (f _)
               (string-match "\\\.el$" f))
             (lambda (d _)
               (not
                (string-match
                 "^\\\..*/$\\|^theme/$\\|^g_.*/$\\|^t_.*/$\\|^private/$" d)))
             option
             t))


(defun make-emacs-source-tags (&optional src-root tags-file)
  "Make TAGS-FILE for Emacs' C and Lisp source code in SRC-ROOT.

Example:
 (make-emacs-source-tags
   (tags-spec->% :emacs-source)
   source-directory
   t)
"
  (interactive)
  (let ((tf (or tags-file (tags-spec->% :emacs-source))))
    ;; make c source tags
    (make-tags (concat (or src-root
                           source-directory)
                       "src/")
               tf
               (lambda (f _) (string-match "\\\.[ch]$" f))
               (lambda (_ __) t)
               nil
               t)
    ;; make elisp source tags
    (make-tags (concat (or src-root
                           source-directory)
                       "lisp/")
               tf
               (lambda (f _) (string-match "\\\.el$" f))
               (lambda (_ __) t)
               nil
               t)))


(defun make-c-tags (home tags-file &optional option renew dir-filter)
  "Make TAGS-FILE for C source code in HOME."
  (make-tags home
             tags-file
             (lambda (f _) (string-match "\\\.[ch]$" f))
             (or dir-filter
                 (lambda (d _)
                   (not
                    (string-match
                     "^\\\.git/$\\|^out/$\\|^objs/$\\|^c\\\+\\\+/$"
                     d))))
             option
             renew))


(defun make-dir-tags (dir store &optional option renew)
  "Make tags for specified DIR."
  (interactive (list (read-directory-name "make tags for ")
                     (read-file-name "store tags in " nil nil nil ".tags")
                     (read-string (concat
                                   (string-trim> tags-program " +.*")
                                   " option: ")
                                  (car *tags-option-history*)
                                  '*tags-option-history*)
                     (y-or-n-p "tags renew? ")))
  (let ((home (path+ (expand-file-name dir))))
    (when (make-tags home
                     store
                     (lambda (f _)
                       (not (string-match
                             "\\\.tags$" f)))
                     (lambda (d _)
                       (not (string-match
                             "^\\\.[_ a-zA-Z]+/$\\|^out/$\\|^build/$" d)))
                     option
                     renew))))


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


(unless-fn% 'xref-find-references 'xref

  (defun xref-find-references (what)
    "Alias of `tags-apropos'."
    (interactive
     (list (read-string "Find references of: "
                        (cdr (symbol@ 'symbol)))))
    (tags-apropos what)))




(provide 'tags)


;; eof
