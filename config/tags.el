;;;; -*- lexical-binding:t -*-
;;
;; TAGS defintion and make
;;




(defmacro v-tags->% (&rest keys)
  "Versionized TAGS file, use `visit-tag-table' to visit"
  (let ((tags `(list
                :emacs-home
                ,(expand-file-name (v-home* ".tags/home/" "TAGS"))
                :emacs-source
                ,(expand-file-name (v-home* ".tags/source/" "TAGS"))
                :os-include
                ,(expand-file-name (v-home* ".tags/os/" "TAGS")))))
    `(self-spec->% ,tags ,@keys)))


(defun make-tags (home tags-file file-filter dir-filter &optional renew)
  "Make tags."
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
                     (eshell-command
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
  "Make TAGS-FILE for Emacs' C and Lisp source code on SRC-ROOT."
  (let ((lisp-ff (lambda (f _) (string-match "\\\.el$" f)))
        (c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (lambda (_ __) t)))
    (when (file-exists-p (concat src-root "src/"))
      (make-tags (concat src-root "src/") tags-file c-ff df renew))
    (when (file-exists-p (concat src-root "lisp/"))
      (make-tags (concat src-root "lisp/") tags-file lisp-ff df))))


(defun make-c-tags (home tags-file &optional renew)
  "Make TAGS-FILE for C source code at HOME."
  (let ((c-ff (lambda (f _) (string-match "\\\.[ch]$" f)))
        (df (lambda (d _) (not
                           (string-match
                            "^\\\.git/$\\|^out/$\\|^objs/$\\|^c\\\+\\\+/$"
                            d)))))
    (make-tags home tags-file c-ff df renew)))


(defmacro os-include-paths ()
  `(let* ((str (platform-supported-if windows-nt
                   nil
                 (eshell-command-result "echo '' | cc -v -E 2>&1 >/dev/null -")))
          (seq (split-string>< str "\n" t "[ \t\r]"))
          (inc (platform-supported-if windows-nt
                   nil
                 (take-while
                  (lambda (p)
                    (string-match "End of search list." p))
                  (drop-while
                   (lambda (p)
                     (string-match "#include <...> search starts here:" p))
                   seq)))))
     (platform-supported-if darwin
         (take-while
          (lambda (p)
            (string-match "/System/Library/Frameworks (framework directory)" p))
          inc)
       inc)))


(defun make-os-c-tags (&optional renew)
  "Make tags for OS include."
  (let ((inc (os-include-paths)))
    (make-c-tags (car inc) (v-tags->% :os-include) renew)
    (dolist (p (cdr inc))
      (make-c-tags p (v-tags->% :os-include)))))
