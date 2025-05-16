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

;;; macro

(eval-when-compile
  (defmacro unless-default-file-name-coding-system% (&rest body)
    (declare (indent 0))
    `(unless% (eq default-file-name-coding-system locale-coding-system)
       ,@body)))

;; end of macro

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
  (when-platform% windows-nt
    ;; prefer GNU find on Windows, such for `find-dired' or `find-name-dired'.
    (when-var% find-program grep
      (let ((find (executable-find*
                   "find"
                   (lambda (find)
                     (let ((ver (shell-command* find "--version")))
                       (and (= 0 (car ver))
                            (string-match "^find (GNU findutils)"
                                          (cdr ver))))))))
        (setq find-program (shell-quote-argument find))))
    ;; make zip.bat
    (unless% (executable-find* "zip")
      ;; on Windows: there are no builtin zip program. zip.bat works
      ;; with `dired-do-compress-to' and `org-odt-export-to-odt'.
      ;; prefer 7z, because 7za less archive formats supported.
      (cond ((executable-find% "7z")
             (dup-file (emacs-home% "config/direds_zip_7z.bat")
                       (v-home% ".exec/zip.bat")))
            ((executable-find% "7za")
             (dup-file (emacs-home% "config/direds_zip_7za.bat")
                       (v-home% ".exec/zip.bat")))
            ((executable-find% "minizip")
             (dup-file (emacs-home% "config/direds_zip_minizip.bat")
                       (v-home% ".exec/zip.bat"))))))
  ;; keys
  (define-key dired-mode-map "b" #'dired*-hexl-find-file)
  (define-key dired-mode-map "B" #'dired*-browse-file)
  (define-key dired-mode-map "w" #'dired*-copy-filename-as-kill)
  (define-key dired-mode-map "W" #'dired*-echo-current-directory)
  (when-version% > 28 (require 'dired-x nil t))
  t)

;; end of `dired' setting

;;; detect-coding-string

(when-platform% windows-nt

  (unless% (eq default-file-name-coding-system locale-coding-system)

    (defun insert-directory* (file switches &optional wildcard full-directory-p)
      "\\=`dired-find-file\\=' should failed when using GNU's ls program on Windows.
       We try to encode multibyte directory name with
       \\=`locale-coding-system\\=' when the multibyte directory name
       encoded with non \\=`locale-coding-system\\='."
      (let ((file (if (multibyte-string-p file)
                      (encode-coding-string file locale-coding-system)
                    file)))
        (funcall '_insert-directory_ file switches wildcard full-directory-p)))

    (defun dired-shell-stuff-it* (command file-list on-each &optional _)
      "\\=`dired-do-shell-command\\=' or \\=`dired-do-async-shell-command\\='
       should failed when open the files which does not been
       encoded with \\=`locale-coding-system\\='."
      (let ((file-list (let ((arg1 file-list) (fs nil))
                         (dolist (x arg1 fs)
                           (append!
                            (if (multibyte-string-p x)
                                (encode-coding-string x locale-coding-system)
                              x)
                            fs
                            delete)))))
        (funcall '_dired-shell-stuff-it_ command file-list on-each)))

    (defun dired-shell-command* (cmd)
      "\\=`dired-do-compress-to\\=' should failed when
       \\=`default-directory\\=' or \\=`dired-get-marked-files\\=' does not
       encoded with \\=`locale-coding-system\\='."
      (let ((cmd (if (multibyte-string-p cmd)
                     (encode-coding-string cmd locale-coding-system)
                   cmd)))
        (funcall '_dired-shell-command_ cmd)))

    (defun dired-compress-file* (file)
      "\\=`dired-compress-file\\=' should failed when FILE arg does not
       encoded with \\=`locale-coding-string\\='."
      (let ((file (if (multibyte-string-p file)
                      (encode-coding-string file locale-coding-system)
                    file)))
        (funcall '_dired-compress-file_ file)))))

;;

;;; `arc-mode'

(eval-when-compile
  (defmacro when-archive-summarize-files% (&rest body)
    (declare (indent 0))
    (when-fn% archive-summarize-files arc-mode
      `(unless-default-file-name-coding-system%
         ,@body))))

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

;;; `dired-aux'

(defun on-dired-aux-init! ()
  "On \\=`dired-aux\\=' initialization."
  ;; on ancient Emacs, `dired' can't recognize .zip archive.
  ;; [! zip x.zip ?] compress marked files to x.zip，
  ;; see `dired-compress-file-suffixes'.
  (when-var% dired-compress-files-suffixes dired-aux
    (when% (and (null (assoc-string "\\.zip\\'" dired-compress-file-suffixes))
                (executable-find* "zip")
                (executable-find* "unzip"))
      (push! '("\\.zip\\'" ".zip" "unzip") dired-compress-file-suffixes)))
  ;; uncompress/compress .7z file
  (when% (or (executable-find* "7z")
             (executable-find* "7za"))
    (let ((7za? (if% (executable-find* "7z") "7z" "7za")))
      (when-var% dired-compress-file-suffixes dired-aux
        ;; [Z] uncompress from .7z
        (let ((uncompress (concat 7za? " x -t7z -aoa -o%o %i")))
          (if% (assoc-string "\\.7z\\'" dired-compress-file-suffixes)
              (setcdr (assoc-string "\\.7z\\'" dired-compress-file-suffixes)
                      (list "" uncompress))
            (push! (list "\\.7z\\'" "" uncompress)
                   dired-compress-file-suffixes)))
        ;; [c] compress to .7z
        (when-fn% dired-do-compress-to dired-aux
          (let ((compress (concat 7za? " a -t7z %o %i")))
            (require 'format-spec)
            (if% (assoc-string "\\.7z\\'" dired-compress-files-alist)
                (setcdr (assoc-string "\\.7z\\'" dired-compress-files-alist)
                        compress)
              (push! (cons "\\.7z\\'" compress)
                     dired-compress-files-alist)))))))
  ;; error at `dired-internal-noselect' on Windows:
  ;; Reading directory: "ls --dired -al -- d:/abc/中文/" exited with status 2
  ;; https://lists.gnu.org/archive/html/emacs-devel/2016-01/msg00406.html
  ;; (setq file-name-coding-system locale-coding-system)
  (when-platform% windows-nt
    (unless% (eq default-file-name-coding-system locale-coding-system)
      (declare-function _insert-directory_ (v-home%> "config/direds") t t)
      (defadvice* '_insert-directory_ 'insert-directory #'insert-directory*)
      (declare-function _dired-shell-stuff-it_ (v-home%> "config/direds") t t)
      (defadvice* '_dired-shell-stuff-it_
        'dired-shell-stuff-it #'dired-shell-stuff-it*)
      (declare-function _dired-shell-command_ (v-home%> "config/direds") t t)
      (defadvice* '_dired-shell-command_
        'dired-shell-command #'dired-shell-command*))
    ;; [Z] to compress or uncompress .gz file
    (when-var% dired-compress-file-suffixes dired-aux
      (when% (or (executable-find% "gzip")
                 (executable-find% "7z")
                 (executable-find% "7za"))
        (when% (assoc-string ":" dired-compress-file-suffixes)
          (setq dired-compress-file-suffixes
                (remove (assoc-string ":" dired-compress-file-suffixes)
                        dired-compress-file-suffixes)))
        (when% (and (null (executable-find* "gunzip"))
                    (or (executable-find* "7z")
                        (executable-find* "7za")))
          (let ((7za? (concat (if (executable-find% "7z") "7z" "7za")
                              " x -tgz -aoa %i")))
            (if% (assoc-string "\\.gz\\'" dired-compress-file-suffixes)
                (setcdr (assoc-string "\\.gz\\'" dired-compress-file-suffixes)
                        (list "" 7za?))
              (push! (cons "\\.gz\\'" 7za?) dired-compress-file-suffixes))))

        (when-fn% dired-compress-file dired-aux
          (declare-function _dired-compress-file_
                            (v-home%> "config/direds") t t)
          (defadvice* '_dired-compress-file_
            'dired-compress-file #'dired-compress-file*))))))

;; end of `dired-aux'

(provide 'direds)

;; end of direds.el
