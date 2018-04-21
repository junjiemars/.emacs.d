;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-tags.el
;;;;


(defmacro tags-spec->% (&rest key)
  "Extract value from the list of spec via KEYS at compile time.

\(tags-spec->% :emacs-home\)
\(tags-spec->% :emacs-source\)
\(tags-spec->% :emacs-os-include\)"
  `(self-spec->% (list
									:emacs-home
									,(expand-file-name (v-home% ".tags/home/" "TAGS"))
									:emacs-source
									,(expand-file-name (v-home% ".tags/source/" "TAGS"))
									:os-include
									,(expand-file-name (v-home% ".tags/os/" "TAGS")))
     ,@key))


(defun make-tags (home tags-file file-filter dir-filter &optional renew)
  "Make tags.

HOME where the source files locate,
TAGS-FILE where the tags file to save,
FILE-FILTER file filter function,
DIR-FILTER directory filter function,
RENEW create tags file when t"
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
                     (shell-command-to-string
                      (format "etags -o %s -l auto -a %s ; echo %s"
                              tags-file f f))))
      (when (file-exists-p tags-file)
        (add-to-list 'tags-table-list tags-dir t #'string=)))))


(defun make-emacs-home-tags (tags-file &optional renew)
  "Make TAGS-FILE for Emacs' home directory."
  (let ((lisp-ff (lambda (f _) (string-match "\\\.el$" f)))
        (home-df
         (lambda (d _)
           (not
            (string-match
             "^\\\..*/$\\|^theme/$\\|^g_.*/$\\|^t_.*/$\\|^private/$" d)))))
    (make-tags emacs-home tags-file lisp-ff home-df renew)))


(defun make-emacs-source-tags (tags-file src-root &optional renew)
  "Make TAGS-FILE for Emacs' C and Lisp source code in SRC-ROOT.

\(make-emacs-source-tags
   \(`tags-spec->%' :emacs-home\)
   `source-directory' t\)"
  (let ((lisp-ff (lambda (f _) (string-match "\\\.el$" f)))
        (c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (lambda (_ __) t)))
    (when (file-exists-p (concat src-root "src/"))
      (make-tags (concat src-root "src/") tags-file c-ff df renew))
    (when (file-exists-p (concat src-root "lisp/"))
      (make-tags (concat src-root "lisp/") tags-file lisp-ff df))))


(defun make-c-tags (home tags-file &optional renew)
  "Make TAGS-FILE for C source code in HOME."
  (let ((c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (lambda (d _) (not
                           (string-match
                            "^\\\.git/$\\|^out/$\\|^objs/$\\|^c\\\+\\\+/$"
                            d)))))
    (make-tags home tags-file c-ff df renew)))


(defun make-system-c-tags (includes &optional renew)
  "Make tags for system INCLUDES.

INCLUDES should be set with `system-cc-include'."
  (make-c-tags (car includes) (tags-spec->% :os-include) renew)
  (dolist (p (cdr includes))
    (make-c-tags p (tags-spec->% :os-include))))


(provide 'use-tags)
