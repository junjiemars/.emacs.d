;;
;; TAGS defintion and make
;;


(defvar vdir-tags-file (expand-file-name (vdir! ".tags/" "TAGS"))
  "Versionized TAGS file, use `visit-tag-table' to visit")


(defmacro make-tags (home tags-file file-filter dir-filter &optional renew)
  "Make tags."
  `(when (file-exists-p ,home)
     (when (and ,renew (file-exists-p ,tags-file))
       (delete-file ,tags-file))
     (dir-files-iterate ,home
                        (when ,file-filter ,file-filter)
                        (when ,dir-filter ,dir-filter)
                        (lambda (f)
                          (eshell-command
                           (format "etags -o %s -l auto -a %s ; echo %s"
                                   ,tags-file f f))))
     (when (and (file-exists-p ,tags-file)
                (not (member (file-name-directory ,tags-file)
                             tags-table-list)))
       (add-to-list 'tags-table-list (file-name-directory ,tags-file)
                    t #'string=))))


(defmacro make-emacs-home-tags (tags-file &optional renew)
  "Make TAGS-FILE for Emacs' home directory."
  `(let ((lisp-ff (lambda (f) (string-match "\\\.el$" f)))
         (home-df (lambda (d) (not (or (string-match "^\\\..*/$" d)
                                       (string-match "^elpa/$" d)
                                       (string-match "^theme/$" d)
                                       (string-match "^g_.*/$" d)
                                       (string-match "^t_.*/$" d)
                                       (string-match "^private/$" d)))))
         (root-c-ff (lambda (f) (string-match "\\\.[ch]$" f)))
         (root-df (lambda (d) t)))
     (make-tags emacs-home ,tags-file lisp-ff home-df ,renew)))


(defmacro make-emacs-source-tags (tags-file src-root &optional renew)
  "Make TAGS-FILE for Emacs' C and Lisp source code on SRC-ROOT."
  `(let ((lisp-ff (lambda (f) (string-match "\\\.el$" f)))
         (c-ff (lambda (f) (string-match "\\\.[ch]$" f)))
         (df (lambda (d) t)))
     (when (file-exists-p (concat ,src-root "src/"))
       (make-tags (concat ,src-root "src/") ,tags-file c-ff df ,renew))
     (when (file-exists-p (concat ,src-root "lisp/"))
       (make-tags (concat ,src-root "lisp/") ,tags-file lisp-ff df ,renew))))
