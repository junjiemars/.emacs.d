;;
;; TAGS defintion and make
;;



(defvar vdir-tags (expand-file-name (make-vdir ".tags/"))
  "Versionized TAGS directory, use `visit-tag-table' to visit")

(defvar vdir-tags-file (concat vdir-tags "TAGS")
  "Versionized TAGS file, use `visit-tag-table' to visit")

(setq tags-table-list (list vdir-tags))


(defun make-emacs-tags (&optional emacs-root)
  "Make `vdir-tags-file' on `emacs-home', if provides `emacs-root' or defined
`source-directory' append tags into `vdir-tags-file'.

\(FN EMACS-SOURCE-ROOT\)"
  (let ((lisp-ff (lambda (f) (string-match "\\\.el$" f)))
        (home-df (lambda (d) (not (or (string-match "^\\\..*/" d)
                                      (string-match "^elpa/$" d)
                                      (string-match "^theme/$" d)
                                      (string-match "^g_.*/$" d)
                                      (string-match "^t_.*/$" d)
                                      (string-match "^private/$" d)))))
        (root-c-ff (lambda (f) (string-match "\\\.[ch]$" f)))
        (root-df (lambda (d) t))
        (fn
         (lambda (f)
           (message "%s" f)
           (eshell-command
            (format "etags -o %s -a %s"
                    (expand-file-name vdir-tags-file) f)))))
    (when (file-exists-p vdir-tags-file)
      (delete-file vdir-tags-file))
    (dir-files-iterate emacs-home lisp-ff home-df fn)
    (let* ((root (or emacs-root
                     (if (boundp 'source-directory)
                         source-directory
                       nil)))
           (c-src (concat root "src/"))
           (lisp-src (concat root "lisp/")))
      (when (file-exists-p c-src)
        (dir-files-iterate c-src root-c-ff root-df fn))
      (when (file-exists-p lisp-src)
        (dir-files-iterate lisp-src lisp-ff root-df fn)))))
