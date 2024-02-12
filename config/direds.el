;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; direds.el
;;;;

;;; macro

(defmacro unless-default-file-name-coding-system% (&rest body)
  (declare (indent 0))
  `(unless% (eq default-file-name-coding-system locale-coding-system)
     ,@body))

;; end of macro

(when-platform% 'windows-nt
  ;; on Windows: there are no builtin zip program
  ;; so try to use minzip in Emacs dep for Windows.
  ;; zip.bat works with `dired-do-compress-to' and `org-odt-export-to-odt'.
  (eval-when-compile

    (defun _make_zip_bat_ (zip &rest ignore)
      "Make ZIP.bat in \\=`exec-path\\=' for minizip or 7z[a]."
      (declare (indent 1))
      (when (stringp zip)
        (save-str-to-file
         (concat
          "@echo off\n"
          (format "REM zip.bat for %s on Windows\n" zip)
          "REM generated by Nore Emacs " (nore-emacs) "\n\n"
          "REM local variable declaration\n\n"
          "setlocal EnableDelayedExpansion\n"
          "\n"
          "set _OPT=%*\n"
          "set _ZIP=\n"
          "set _ARGV=\n"
          "\n"
          "REM parsing command line arguments\n\n"
          ":getopt\n"
          (cond ((string= "minizip" zip)
                 "if \"%1\"==\"-mX0\" set _OPT=%_OPT:-mX0=-0% & shift & goto :getopt\n")
                ((string-match "7za?" zip)
                 (concat
                  "if \"%1\"==\"-mX0\" set _OPT=%_OPT:-mX0=-mx0% & shift & goto :getopt\n"
                  "if \"%1\"==\"-0\" set _OPT=%_OPT:-0=-mx0% & shift & goto :getopt\n"
                  "if \"%1\"==\"-9\" set _OPT=%_OPT:-9=-mx9% & shift & goto :getopt\n")))
          "\n"
          "REM ignore options\n"
          (let ((options nil))
            (dolist* (x (cond ((string= "minizip" zip)
                               (append '("-r" "--filesync" "-rmTq") ignore))
                              ((string-match "7za?" zip)
                               (append '("-r" "--filesync" "-rmTq"))))
                        options)
              (setq options
                    (concat options
                            (format "if \"%%1\"==\"%s\" set _OPT=%%_OPT:%s=%% & shift & goto :getopt\n" x x)))))
          "\n"
          "REM extract zip and argv\n"
          "if not \"%1\"==\"\" (\n"
          "  if \"%_ZIP%\"==\"\" (\n"
          "    if \"%_ARGV%\"==\"\" (\n"
          "      set _ZIP=%1\n"
          "    )\n"
          "  ) else (\n"
          "    set _ARGV=%_ARGV% %1\n"
          "  )\n"
          "  set _OPT=!_OPT:%1=!\n"
          "  shift\n"
          "  goto :getopt\n"
          ")\n\n"
          (cond ((string-match "7za?" zip)
                 (let ((7z (string-match* "7za?" zip 0)))
                   (format (concat "REM %s call\n"
                                   "%s a %%_OPT%% -tzip -- %%_ZIP%% %%_ARGV%%\n"
                                   "if exist %%_ZIP%% (\n"
                                   "  %s d %%_OPT%% -tzip -- %%_ZIP%% %%_ZIP%%\n"
                                   ")\n")
                           7z 7z 7z)))
                ((string= "minizip" zip)
                 (concat "REM minizip recursive call\n\n"
                         "call :loop %_ARGV%\n"
                         "goto :end\n"
                         "\n:zip\n"
                         "set _file=%1\n"
                         "set _file=%_file:./=%\n"
                         "if not \"%_file%\"==\"%_ZIP%\" (\n"
                         "  if exist %_ZIP% (\n"
                         "    minizip %_OPT% -a %_ZIP% %_file%\n"
                         "  ) else (\n"
                         "    minizip %_OPT% %_ZIP% %_file%\n"
                         "  )\n"
                         ")\n"
                         "goto :end\n"
                         "\n:loop\n"
                         "for %%i in (%*) do (\n"
                         "  if exist \"%%i/*\" (\n"
                         "    for %%f in (%%i/*) do (\n"
                         "      call :loop %%i/%%f\n"
                         "    )\n"
                         "    for /d %%d in (%%i/*) do (\n"
                         "      call :loop %%i/%%d\n"
                         "    )\n"
                         "  ) else (\n"
                         "    call :zip %%i\n"
                         "  )\n"
                         ")\n"
                         "\n:end\n"))))
         (v-home% ".exec/zip.bat"))))

    (unless (executable-find% "zip")
      ;; zip external program
      ;; prefer 7z, because 7za less archive formats supported
      (cond ((executable-find% "7z") (_make_zip_bat_ "7z"))
            ((executable-find% "7za") (_make_zip_bat_ "7za"))
            ((executable-find% "minizip") (_make_zip_bat_ "minizip"))))))


(defun dired*-echo-current-directory (&optional localp)
  "Echo the current directory in \\=`dired-mode\\='."
  (interactive)
  (let ((d (dired-current-directory localp))
        (interprogram-paste-function nil))
    (kill-new d)
    (message "%s" d)))


(defun browse-file ()
  "Browse the file using external browser."
  (interactive)
  (declare-function browse-url-default-browser "browse-url" t)
  (let ((browse-url-browser-function #'browse-url-default-browser)
        (name (if (eq 'dired-mode major-mode)
                  (expand-file-name (dired-get-file-for-visit))
                (buffer-file-name))))
    (and name (browse-url name))))


(defun dired-hexl-find-file ()
  "Edit the current file as hex dump format in \\=`dired-mode\\='."
  (interactive)
  (hexl-find-file
   (if-fn% 'dired-get-file-for-visit 'dired
           (dired-get-file-for-visit)
     (or (dired-get-filename nil t)
         (user-error "%s" "No file on this line")))))

(defun dired*-copy-filename-as-kill (&optional arg)
  "See \\=`dired-copy-filename-as-kill\\='."
  (interactive "P")
  (let ((interprogram-paste-function nil))
    (dired-copy-filename-as-kill arg)))

(defun on-dired-init! ()
  "On \\=`dired\\=' initialization."
  ;; prefer GNU's ls (--dired option) on Windows or Darwin. on
  ;; Windows: `dired-mode' does not display executable flag in file
  ;; mode，see `dired-use-ls-dired' and `ido-dired' for more defails
  (when% (executable-find%
          "ls"
          (lambda (bin)
            (let ((home (shell-command* bin (emacs-home*))))
              (zerop (car home)))))
    ;; on Drawin: the builtin ls does not support --dired option
    (setq% dired-use-ls-dired
           (executable-find%
            "ls"
            (lambda (bin)
              (let ((dired (shell-command* bin "--dired")))
                (zerop (car dired)))))
           'dired)
    ;; using `insert-directory-program'
    (setq% ls-lisp-use-insert-directory-program t 'ls-lisp)
)

  (when-platform% 'windows-nt
    ;; prefer GNU find on Windows, such for `find-dired' or `find-name-dired'.
    (let ((find (executable-find%
                 "find"
                 (lambda (bin)
                   (let ((ver (shell-command* bin "--version")))
                     (and (zerop (car ver))
                          (string-match "^find (GNU findutils)"
                                        (cdr ver))))))))
      (when find
        (windows-nt-env-path+ (file-name-directory find)))))

  (define-key dired-mode-map (kbd "b") #'dired-hexl-find-file)
  (define-key dired-mode-map (kbd "B") #'browse-file)
  (define-key dired-mode-map (kbd "w") #'dired*-copy-filename-as-kill)
  (define-key dired-mode-map (kbd "W") #'dired*-echo-current-directory))

;; end of `dired' setting

;;; detect-coding-string

(when-platform% 'windows-nt

  (unless% (eq default-file-name-coding-system locale-coding-system)

    (defadvice insert-directory (before insert-directory-before compile)
      "\\=`dired-find-file\\=' should failed when using GNU's ls program on Windows.
       We try to encode multibyte directory name with
       \\=`locale-coding-system\\=' when the multibyte directory name
       encoded with non \\=`locale-coding-system\\='."
      (when (multibyte-string-p (ad-get-arg 0))
        (ad-set-arg 0 (encode-coding-string (ad-get-arg 0)
                                            locale-coding-system))))

    (defadvice dired-shell-stuff-it (before dired-shell-stuff-it-before disable)
      "\\=`dired-do-shell-command\\=' or \\=`dired-do-async-shell-command\\='
       should failed when open the files which does not been
       encoded with \\=`locale-coding-system\\='."
      (ad-set-arg 1 (let ((arg1 (ad-get-arg 1))
                          (files nil))
                      (dolist* (x arg1 files)
                        (append! (if (multibyte-string-p x)
                                     (encode-coding-string
                                      x
                                      locale-coding-system)
                                   x)
                                 files t)))))

    (defadvice dired-shell-command (before dired-shell-command-before disable)
      "\\=`dired-do-compress-to\\=' should failed when
       \\=`default-directory\\=' or \\=`dired-get-marked-files\\=' does not
       encoded with \\=`locale-coding-system\\='."
      (let ((arg0 (ad-get-arg 0)))
        (when (multibyte-string-p arg0)
          (ad-set-arg 0 (encode-coding-string arg0 locale-coding-system)))))

    (defadvice dired-compress-file (before dired-compress-file-before disable)
      "\\=`dired-compress-file\\=' should failed when FILE arg does not
       encoded with \\=`locale-coding-string\\='."
      (let ((arg0 (ad-get-arg 0)))
        (when (multibyte-string-p arg0)
          (ad-set-arg 0 (encode-coding-string arg0 locale-coding-system)))))))

;;

;;; `arc-mode'

(defmacro when-fn-archive-summarize-files% (&rest body)
  (declare (indent 0))
  (when-fn% 'archive-summarize-files 'arc-mode
    `(unless-default-file-name-coding-system%
       ,@body)))

(when-fn-archive-summarize-files%
  (defadvice archive-summarize-files
      (before archive-summarize-files-before first compile disable)
    "\\=`archive-summarize-files\\=' may not display file name in right
       coding system."
    (let ((arg0 (ad-get-arg 0))
          (files nil))
      (when (consp arg0)
        (ad-set-arg
         0
         (dolist* (x arg0 files)
           (when (and (arrayp x) (= 3 (length x)))
             (let ((decode (substring-no-properties
                            (decode-coding-string
                             (aref x 0) locale-coding-system))))
               (aset x 0 decode)
               (aset x 2 (length decode))))
           (append! x files t)))))))

(defun on-arc-mode-init! ()
  (when-fn-archive-summarize-files%
    (ad-enable-advice #'archive-summarize-files 'before
                      "archive-summarize-files-before")
    (ad-activate #'archive-summarize-files t)))

;; end of `arc-mode'

;;; `dired-aux'

(defun on-dired-aux-init! ()
  "On \\=`dired-aux\\=' initialization."
  ;; on ancient Emacs, `dired' can't recognize .zip archive.
  ;; [! zip x.zip ?] compress marked files to x.zip，
  ;; see `dired-compress-file-suffixes'.
  (when-var% dired-compress-files-suffixes 'dired-aux
    (when% (and (not (assoc-string "\\.zip\\'" dired-compress-file-suffixes))
                (executable-find% "zip")
                (executable-find% "unzip"))
      (push! '("\\.zip\\'" ".zip" "unzip") dired-compress-file-suffixes)))
  ;; uncompress/compress .7z file
  (when% (or (executable-find% "7z")
             (executable-find% "7za"))
    (let ((7za? (if (executable-find% "7z") "7z" "7za")))
      (when-var% dired-compress-file-suffixes 'dired-aux
        ;; [Z] uncompress from .7z
        (let ((uncompress (concat 7za? " x -t7z -aoa -o%o %i")))
          (if% (assoc-string "\\.7z\\'" dired-compress-file-suffixes)
              (setcdr (assoc-string "\\.7z\\'" dired-compress-file-suffixes)
                      (list "" uncompress))
            (push! (list "\\.7z\\'" "" uncompress)
                   dired-compress-file-suffixes)))
        ;; [c] compress to .7z
        (when-fn% 'dired-do-compress-to 'dired-aux
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
  (when-platform% 'windows-nt
    (unless% (eq default-file-name-coding-system locale-coding-system)
      (ad-enable-advice #'dired-shell-stuff-it 'before
                        "dired-shell-stuff-it-before")
      (ad-enable-advice #'dired-shell-command 'before
                        "dired-shell-command-before")
      (ad-activate #'dired-shell-stuff-it t)
      (ad-activate #'dired-shell-command t))
    ;; [Z] to compress or uncompress .gz file
    (when-var% dired-compress-file-suffixes 'dired-aux
      (when% (or (executable-find% "gzip")
                 (executable-find% "7z")
                 (executable-find% "7za"))
        (when% (assoc-string ":" dired-compress-file-suffixes)
          (setq dired-compress-file-suffixes
                (remove (assoc-string ":" dired-compress-file-suffixes)
                        dired-compress-file-suffixes)))
        (when% (and (not (executable-find% "gunzip"))
                    (or (executable-find% "7z")
                        (executable-find% "7za")))
          (let ((7za? (concat (if (executable-find% "7z") "7z" "7za")
                              " x -tgz -aoa %i")))
            (if% (assoc-string "\\.gz\\'" dired-compress-file-suffixes)
                (setcdr (assoc-string "\\.gz\\'" dired-compress-file-suffixes)
                        (list "" 7za?))
              (push! (cons "\\.gz\\'" 7za?) dired-compress-file-suffixes))))

        (when-fn% 'dired-compress-file 'dired-aux
          (ad-enable-advice #'dired-compress-file 'before
                            "dired-compress-file-before")
          (ad-activate #'dired-compress-file t))))))

;; end of `dired-aux'

(provide 'direds)

;; end of direds.el
