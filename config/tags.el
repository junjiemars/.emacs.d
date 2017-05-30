;;
;; TAGS defintion and make
;;



;; Versionized TAGS directories, use `visit-tag-table' to visit
(defvar vdir-tags (expand-file-name (make-vdir ".tags/")))
(setq tags-table-list (list vdir-tags))
(defvar vdir-tags-file (concat vdir-tags "TAGS"))




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
            (format "etags -o %s -a %s" vdir-tags-file f)))))
    (when (file-exists-p vdir-tags-file)
      (delete-file vdir-tags-file))
    (dir-files-iterate emacs-home lisp-ff home-df fn)
    (let* ((root (or emacs-root source-directory))
           (c-src (concat root "src/"))
           (lisp-src (concat root "lisp/")))
      (when (file-exists-p c-src)
        (dir-files-iterate c-src root-c-ff root-df fn))
      (when (file-exists-p lisp-src)
        (dir-files-iterate lisp-src lisp-ff root-df fn)))))


(defmacro append-etags-paths (paths)
  `(when ,paths
     (let ((s (concat (format " -path \"%s\"" (car ,paths)))))
       (dolist (p (cdr, paths))
         (setq s (concat s (format " -o -path \"%s\"" p))))
       s)))

(defmacro append-etags-names (names)
  `(when ,names
     (let ((s (concat (format " -name \"%s\"" (car ,names)))))
       (dolist (n (cdr ,names))
         (setq s (concat s (format " -o -name \"%s\"" n))))
       s)))

(defmacro build-etags-lisp-command (find xargs dir tags &optional paths names)
  `(let ((f "%s %s %s %s | %s etags -o %s -a "))
     (format f
             ,find
             ,dir
             (if ,paths ,paths "")
             (if ,names ,names "")
             ,xargs
             ,tags)))

(defmacro build-etags-c-command (find xargs dir tags &optional names)
  `(let ((f "%s %s -type f \\\( %s \\\) | %s etags -o %s -a"))
     (format f
             ,find
             ,dir
             (if ,names ,names "")
             ,xargs
             ,tags)))


(defun make-emacs-etags (&optional emacs-src emacs-lisp)
  "Make tags of `emacs-home' via etags."
  (let ((tags (concat vdir-tags "TAGS"))
        (find-bin (platform-supported-if windows-nt "/usr/bin/find" "find"))
        (xargs-bin (platform-supported-if windows-nt "/usr/bin/xargs" "xargs")))
    (when (file-exists-p tags) (delete-file tags))
    (safe-do-when set-windows-nt-shell (set-windows-nt-shell))
    (shell-command
     (build-etags-lisp-command
      find-bin
      xargs-bin
      emacs-home
      tags
      (format
       "\\\( %s \\\) -prune "
       (append-etags-paths '("*/.git" "*/elpa" "*/g_*" "*/t_*")))
      (format
       "-o \\\( %s \\\)"
       (append-etags-names '("*.el")))))
    (when (and emacs-src (file-exists-p emacs-src))
      (shell-command
       (message "%s"
                (build-etags-c-command
                 find-bin
                 xargs-bin
                 emacs-src
                 tags
                 (append-etags-names '("*.c" "*.h"))))))
    (when (and emacs-lisp (file-exists-p emacs-lisp))
      (shell-command
       (build-etags-lisp-command
        find-bin
        xargs-bin
        emacs-lisp
        tags
        nil
        (append-etags-names '("*.el")))))))
