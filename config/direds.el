;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; direds.el
;;;;

;;; require

(declare-function browse-url-default-browser "browse-url")
(declare-function dired-current-directory "dired")
(autoload 'dired-current-directory "dired")

;; end of require

;;; env

(eval-when-compile
  (defmacro unless-default-file-name-coding-system% (&rest body)
    (declare (indent 0))
    `(unless% (eq default-file-name-coding-system locale-coding-system)
       ,@body)))

(eval-when-compile
  (defmacro when-archive-summarize-files% (&rest body)
    (declare (indent 0))
    (when-fn% archive-summarize-files arc-mode
      `(unless-default-file-name-coding-system%
         ,@body))))

(eval-when-compile
  (when-platform% windows-nt
    (defmacro unless-filename/locale-coding-system% (&rest body)
      (if (eq default-file-name-coding-system locale-coding-system)
          `(comment ,@body)
        `(progn% ,@body)))))

 ;; end of env

;;;
;; `dired'
;;;

(defun dired*-echo-current-directory (&optional localp)
  "Echo the current directory in \\=`dired-mode\\='."
  (interactive)
  (let ((d (dired-current-directory localp))
        (interprogram-paste-function nil))
    (kill-new d)
    (message "%s" d)))

(defun dired*-get-filename ()
  (if-fn% dired-get-file-for-visit dired
          (dired-get-file-for-visit)
    (or (dired-get-filename nil t)
        (user-error "%s" "No file on this line"))))

(when-platform% darwin
  (when-version% <= 28
    (defun browse-url-default-macosx-browser* (url &optional _)
      (interactive)
      (start-process (concat "open " url) nil "open" url))))

(defun dired*-browse-file ()
  "Browse the file using external browser."
  (interactive)
  (let ((browse-url-browser-function #'browse-url-default-browser)
        (name (if (eq 'dired-mode major-mode)
                  (dired*-get-filename)
                (buffer-file-name))))
    (and name
         (if-platform% darwin
             (let ((fn (symbol-function 'browse-url-default-macosx-browser)))
               (unwind-protect
                   (progn
                     (fset 'browse-url-default-macosx-browser
                           #'browse-url-default-macosx-browser*)
                     (browse-url name))
                 (fset 'browse-url-default-macosx-browser fn)))
           (browse-url name)))))


(defun dired*-hexl-find-file ()
  "Edit the current file as hex dump format in \\=`dired-mode\\='."
  (interactive)
  (hexl-find-file (dired*-get-filename)))

(defun dired*-copy-filename-as-kill (&optional arg)
  "See \\=`dired-copy-filename-as-kill\\='."
  (interactive "P")
  (let ((interprogram-paste-function nil))
    (dired-copy-filename-as-kill arg)))

(defun on-dired-init! ()
  "On \\=`dired\\=' initialization."
  (when-var% dired-use-ls-dired dired
    ;; prefer GNU's ls (--dired option) on Windows or Darwin. on
    ;; Windows: `dired-mode' does not display executable flag in file
    ;; mode，see `dired-use-ls-dired' and `ido-dired' for more defails
    ;; on Drawin: the builtin `ls' does not support --dired option.
    (let ((ls (executable-find* "ls")))
      (if ls
          (if (= 0 (car (shell-command* ls "--dired")))
              (set-default 'dired-use-ls-dired t)
            (set-default 'dired-use-ls-dired nil)
            (set-default 'ls-lisp-use-insert-directory-program t))
        (set-default 'dired-use-ls-dired nil)
        (set-default 'ls-lisp-use-insert-directory-program nil))))
  ;; keys
  (define-key dired-mode-map "b" #'dired*-hexl-find-file)
  (define-key dired-mode-map "B" #'dired*-browse-file)
  (define-key dired-mode-map "w" #'dired*-copy-filename-as-kill)
  (define-key dired-mode-map "W" #'dired*-echo-current-directory)
  (when-version% > 28 (require 'dired-x nil t))
  t)

;; end of `dired'

;;;
;; `arc-mode'
;;;

(when-archive-summarize-files%
  (defun archive-summarize-files* (files)
    "\\=`archive-summarize-files\\=' may not display file name."
    (let ((files (if (consp files)
                     (let ((fs nil))
                       (dolist (x files fs)
                         (when (and (arrayp x) (= 3 (length x)))
                           (let ((decode (substring-no-properties
                                          (decode-coding-string
                                           (aref x 0) locale-coding-system))))
                             (aset x 0 decode)
                             (aset x 2 (length decode))))
                         (append! x fs delete)))
                   files)))
      (funcall (symbol-function '_archive-summarize-files_) files))))

(defun on-arc-mode-init! ()
  (when-archive-summarize-files%
    (defadvice* '_archive-summarize-files_
      'archive-summarize-files #'archive-summarize-files*)))

;; end of `arc-mode'

;;;
;; coding
;;;

(when-platform% windows-nt
  (unless-filename/locale-coding-system%
   ;; `dired-find-file' should failed when using GNU's ls program on
   ;; Windows. We try to encode multibyte directory name with
   ;; `locale-coding-system' when the multibyte directory name encoded
   ;; with non `locale-coding-system'.
   (defun insert-directory* (file switches &optional wildcard full-directory-p)
     (let ((file (if (multibyte-string-p file)
                     (encode-coding-string file locale-coding-system)
                   file)))
       (funcall '_insert-directory_ file switches wildcard full-directory-p)))))

(when-platform% windows-nt
  (unless-filename/locale-coding-system%
   ;; `dired-do-shell-command' or `dired-do-async-shell-command'
   ;; should failed when open the files which does not been encoded
   ;; with `locale-coding-system'."
   (defun dired-shell-stuff-it* (command file-list on-each &optional _)
     (let ((file-list (let ((arg1 file-list) (fs nil))
                        (dolist (x arg1 fs)
                          (append!
                           (if (multibyte-string-p x)
                               (encode-coding-string x locale-coding-system)
                             x)
                           fs
                           delete)))))
       (funcall '_dired-shell-stuff-it_ command file-list on-each)))))

(when-platform% windows-nt
  (unless-filename/locale-coding-system%
   ;; `dired-do-compress-to' should failed when `default-directory' or
   ;; `dired-get-marked-files' does not encoded with
   ;; `locale-coding-system'.
   (defun dired-shell-command* (cmd)
     (let ((cmd (if (multibyte-string-p cmd)
                    (encode-coding-string cmd locale-coding-system)
                  cmd)))
       (funcall '_dired-shell-command_ cmd)))))

(when-platform% windows-nt
  (unless-filename/locale-coding-system%
   ;; `dired-compress-file' should failed when FILE arg does not
   ;; encoded with `locale-coding-string'.
   (defun dired-compress-file* (file)
     (let ((file (if (multibyte-string-p file)
                     (encode-coding-string file locale-coding-system)
                   file)))
       (funcall '_dired-compress-file_ file)))))

;; end of coding

;;;
;; `dired-aux'
;;;

(when-platform% windows-nt
  (defun dired*-decompre-file-rule (suffix program rule)
    (when-var% dired-compress-file-suffixes dired-aux
      (when (executable-find* program)
        (let* ((re (format "\\%s\\'" suffix))
               (a1 (assoc-string re dired-compress-file-suffixes)))
          (cond (a1 (setcdr a1 (list "" rule)))
                (t (push! (cons re rule)
                          dired-compress-file-suffixes
                          delete))))))))

(when-platform% windows-nt
  (defun dired*-compress-file-rule (suffix program rule)
    (when-var% dired-compress-files-alist dired-aux
      (when (executable-find* program)
        (let* ((re (format "\\%s\\'" suffix))
               (a1 (assoc-string re dired-compress-files-alist)))
          (cond (a1 (setcdr a1 rule))
                (t (push! (cons re rule)
                          dired-compress-files-alist
                          delete))))))))

(defun on-dired-aux-init! ()
  "On \\=`dired-aux\\=' initialization."
  (when-platform% windows-nt
    ;; coding system
    (unless-filename/locale-coding-system%
     (when-fn% insert-directory nil
       (declare-function _insert-directory_ (v-home%> "config/direds") t t)
       (defadvice* '_insert-directory_
         'insert-directory #'insert-directory*))
     (when-fn% dired-shell-stuff-it dired-aux
       (declare-function _dired-shell-stuff-it_ (v-home%> "config/direds") t t)
       (defadvice* '_dired-shell-stuff-it_
         'dired-shell-stuff-it #'dired-shell-stuff-it*))
     (when-fn% dired-shell-command dired-aux
       (declare-function _dired-shell-command_ (v-home%> "config/direds") t t)
       (defadvice* '_dired-shell-command_
         'dired-shell-command #'dired-shell-command*))
     (when-fn% dired-compress-file dired-aux
       (declare-function _dired-compress-file_ (v-home%> "config/direds") t t)
       (defadvice* '_dired-compress-file_
         'dired-compress-file #'dired-compress-file*)))
    ;; [Z] remove default compress rule
    ;; bug: using ":" to match the target file sucks.
    ;; use [c] `dired-do-compress-to' to compress directory instead.
    (when% (assoc-string ":" dired-compress-file-suffixes)
      (setq dired-compress-file-suffixes
            (remove (assoc-string ":" dired-compress-file-suffixes)
                    dired-compress-file-suffixes)))
    ;; [Z] .gz: decompress, prefer GNU's gzip
    (unless (dired*-decompre-file-rule ".gz" "gzip" "gzip -d %i")
      (dired*-decompre-file-rule ".gz" "7za" "7za x -aoa %i"))
    ;; [c] .zip: compress, `dired-do-compress-to' or `org-odt-export-to-odt'.
    (unless% (executable-find% "zip")
      (unless (dired*-compress-file-rule ".zip" "7z" "7z a -tzip %o %i")
        (dired*-compress-file-rule ".zip" "7za" "7za a -tzip %o %i")))
    ;; required by `dired-aux', it sucks.
    (autoload 'format-spec "format-spec"))
  t)

;; end of `dired-aux'

(provide 'direds)

;; end of direds.el
