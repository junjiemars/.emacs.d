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

(defun make-emacs-etags (&optional renew &optional emacs-src)
  "Make tags of `emacs-home' via etags."
  (let ((tags (concat vdir-tags "TAGS")))
    (when (and renew (file-exists-p tags))
      (delete-file tags))
    (shell-command
     (format
      "find %s \\\( %s \\\) -prune -o \\\( %s \\\) | xargs etags -o %s -a "
      emacs-home
      (append-etags-paths '("*/.git" "*/elpa" "*/g_*" "*/t_*"))
      (append-etags-names '("*.el"))
      tags))
    (when (and emacs-src (file-exists-p emacs-src))
      (shell-command
       (format
        "find %s -type f \\\( %s \\\) | xargs etags -o %s -a"
        emacs-src
        (append-etags-names '("*.c" "*.h"))
        tags)))))
