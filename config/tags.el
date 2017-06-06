;;
;; TAGS defintion and make
;;


(defvar vdir-tags-file (concat (expand-file-name (vdir! ".tags/")) "TAGS")
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
       (push (file-name-directory ,tags-file) tags-table-list))))


(defun make-emacs-tags (tags-file &optional emacs-root)
  "Make `vdir-tags-file' on `emacs-home', if provides `emacs-root' or defined
`source-directory' append tags into `tags-file-name'."
  (let ((lisp-ff (lambda (f) (string-match "\\\.el$" f)))
        (home-df (lambda (d) (not (or (string-match "^\\\..*/$" d)
                                      (string-match "^elpa/$" d)
                                      (string-match "^theme/$" d)
                                      (string-match "^g_.*/$" d)
                                      (string-match "^t_.*/$" d)
                                      (string-match "^private/$" d)))))
        (root-c-ff (lambda (f) (string-match "\\\.[ch]$" f)))
        (root-df (lambda (d) t)))
    (make-tags emacs-home tags-file lisp-ff home-df t)
    (let ((root (or emacs-root
                    (if (boundp 'source-directory)
                        source-directory
                      nil)))
          (lisp-src ))
      (when (file-exists-p (concat root "src/"))
        (make-tags (concat root "src/") tags-file root-c-ff root-df))
      (when (file-exists-p (concat root "lisp/"))
        (make-tags (concat root "lisp/") tags-file lisp-ff root-df)))))
