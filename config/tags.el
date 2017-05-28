;;
;; TAGS defintion and make
;;



;; Versionized TAGS directories, use `visit-tag-table' to visit
(defvar vdir-tags (expand-file-name (make-vdir ".tags/")))
(setq tags-table-list (list vdir-tags))


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

(defun make-emacs-etags (&optional emacs-src emacs-lisp)
  "Make tags of `emacs-home' via etags."
  (let ((tags (concat vdir-tags "TAGS"))
        (lisp-src-format
         "find %s %s %s | xargs etags -o %s -a ")
        (c-src-format
         "find %s -type f \\\( %s \\\) | xargs etags -o %s -a"))
    (when (file-exists-p tags) (delete-file tags))
    (shell-command
     (format
      lisp-src-format
      emacs-home
      (format
       "\\\( %s \\\) -prune "
       (append-etags-paths '("*/.git" "*/elpa" "*/g_*" "*/t_*")))
      (format
       "-o \\\( %s \\\)"
       (append-etags-names '("*.el")))
      tags))
    (when (and emacs-src (file-exists-p emacs-src))
      (shell-command
       (format
        c-src-format
        emacs-src
        (append-etags-names '("*.c" "*.h"))
        tags)))
    (when (and emacs-lisp (file-exists-p emacs-lisp))
      (shell-command
       (format
        lisp-src-format
        emacs-lisp
        ""
        (append-etags-names '("*.el"))
        tags)))))
