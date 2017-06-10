;;
;; TAGS defintion and make
;;


(defvar vdir-tags-files
  (list :emacs-home (expand-file-name (v-home* ".tags/home/" "TAGS"))
        :emacs-source (expand-file-name (v-home* ".tags/source/" "TAGS")))
  "Versionized TAGS file, use `visit-tag-table' to visit")


(defun make-tags (home tags-file file-filter dir-filter &optional renew)
  "Make tags."
  (when (file-exists-p home)
    (let ((tags-dir (file-name-directory tags-file)))
      (if (file-exists-p tags-file)
          (when renew (delete-file tags-file))
        (when (not (file-exists-p tags-dir))
          (make-directory tags-dir t)))
      (dir-iterate home
                   (when file-filter file-filter)
                   (when dir-filter dir-filter)
                   (lambda (f)
                     (eshell-command
                      (format "etags -o %s -l auto -a %s ; echo %s"
                              tags-file f f))))
      (when (file-exists-p tags-file)
        (add-to-list 'tags-table-list tags-dir t #'string=)))))


(defun make-emacs-home-tags (tags-file &optional renew)
  "Make TAGS-FILE for Emacs' home directory."
  (let ((lisp-ff (lambda (f a) (string-match "\\\.el$" f)))
        (home-df
         (lambda (d a)
           (not
            (string-match
             "^\\\..*/$\\|^theme/$\\|^g_.*/$\\|^t_.*/$\\|^private/$" d)))))
    (make-tags emacs-home tags-file lisp-ff home-df renew)))


(defun make-emacs-source-tags (tags-file src-root &optional renew)
  "Make TAGS-FILE for Emacs' C and Lisp source code on SRC-ROOT."
  (let ((lisp-ff (lambda (f a) (string-match "\\\.el$" f)))
        (c-ff (lambda (f a) (string-match "\\\.[ch]$" f)))
        (df (lambda (d a) t)))
    (when (file-exists-p (concat src-root "src/"))
      (make-tags (concat src-root "src/") tags-file c-ff df renew))
    (when (file-exists-p (concat src-root "lisp/"))
      (make-tags (concat src-root "lisp/") tags-file lisp-ff df))))
