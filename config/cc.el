;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cc.el
;;;;
;; Commentary: essential C programming
;;;;

;;; require

;; `shell-format-buffer'
(require% 'ed (v-home%> "config/ed"))
;; `(tags-spec->% :root)'
(require% 'tags (v-home%> "config/tags"))
(require% 'ssh (v-home%> "config/ssh"))

;; end of require

;;;
;; msvc host environment
;;;

(when-platform% windows-nt
  (defun cc*-check-vcvarsall-bat ()
    "Return the path of vcvarsall.bat if which exists."
    (let* ((pfroot (posix-path (getenv "PROGRAMFILES")))
           (vsroot (concat pfroot " (x86)/Microsoft Visual Studio/"))
           (vswhere (concat vsroot "Installer/vswhere.exe")))
      (when (file-exists-p vswhere)
        (posix-path
         (or (let* ((rc (shell-command* (shell-quote-argument vswhere)
                          "-nologo -latest -property installationPath"))
                    (bat (and (zerop (car rc))
                              (concat
                               (string-trim> (cdr rc))
                               "/VC/Auxiliary/Build/vcvarsall.bat"))))
               (when (file-exists-p bat) bat))
             (let* ((ver (car (directory-files
                               vsroot
                               t "[0-9]+" #'string-greaterp)))
                    (bat (concat
                          ver
                          "/BuildTools/VC/Auxiliary/Build/vcvarsall.bat")))
               (when (file-exists-p bat) bat))))))))

(when-platform% windows-nt
  (defun cc*-make-env-bat ()
    "Make cc-env.bat in \\=`exec-path\\='."
    (let ((vcvarsall (cc*-check-vcvarsall-bat))
          (arch (downcase (platform-arch))))
      (when (and vcvarsall arch)
        (save-str-to-file
         (concat "@echo off\n"
                 (concat "rem generated by Nore Emacs" (nore-emacs) "\n\n")
                 "pushd %cd%\n"
                 "cd /d \"" (file-name-directory vcvarsall) "\"\n"
                 "\n"
                 "call vcvarsall.bat " arch "\n"
                 "set CC=cl" "\n"
                 "set AS=ml" (if (string-match "[_a-zA-Z]*64" arch)
                                 "64"
                               "")
                 "\n"
                 "\n"
                 "popd\n"
                 "echo \"%INCLUDE%\"\n")
         (v-home% ".exec/cc-env.bat"))))))

;; msvc host environment

;;;
;; CC
;;;

(defun cc*-cc-check ()
  (let ((cx (if-platform% windows-nt
                '("cc-env.bat" "cl" "gcc")
              '("cc" "gcc" "clang")))
        (o (make-temp-file "cc-c-" nil
                           (if-platform% windows-nt
                               ".exe"
                             ".out")))
        (i (save-str-to-file
            (concat
             "int main(void) {\n"
             "  return 0;\n"
             "}")
            (make-temp-file "cc-s-" nil ".c"))))
    (catch 'br
      (dolist (cc cx)
        (let ((rc (shell-command*
                      (format (if-platform% windows-nt
                                  (if (string= "cc-env.bat" cc)
                                      (concat "%s %s -Fe%s -Fo")
                                    "%s %s -o%s")
                                "%s %s -o%s")
                              (if-platform% windows-nt
                                  (if (string= "cc-env.bat" cc)
                                      "cc-env.bat && cl"
                                    cc)
                                cc)
                              i o))))
          (when (zerop (car rc))
            (throw 'br cc)))))))

(defalias 'cc*-cc
  (let ((b (cc*-cc-check)))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "The name of C compiler executable.")

;; end of CC

;;;
;; xargs
;;;

(when-platform% windows-nt
  (defun cc*-make-xargs-bin ()
    "Make a GNU's xargs alternation in \\=`exec-path\\='."
    (let* ((c (make-temp-file "cc-xargs-" nil ".c"))
           (exe (v-home% ".exec/xargs.exe"))
           (cc (concat (cc*-cc)
                       " -nologo -DNDEBUG=1 -O2 -utf-8"
                       " " c
                       " -Fo" temporary-file-directory
                       " -Fe" exe
                       " -link -release")))
      (when (save-str-to-file
             (concat "#include <stdio.h>\n"
                     "int main(int argc, char **argv) {\n"
                     "  int ch;\n"
                     "  while (EOF != (ch = fgetc(stdin))) {\n"
                     "    fputc(ch, stdout);\n"
                     "  }\n"
                     "  if (ferror(stdin)) {\n"
                     "    perror(\"read failed from stdin\");\n"
                     "    return 1;\n"
                     "  }\n"
                     "  return 0;\n"
                     "}\n")
             c)
        (let ((rc (shell-command* cc)))
          (when (zerop (car rc))
            (file-name-nondirectory exe)))))))

(when-platform% windows-nt
  (defconst +cc*-xargs-bin+
    (file-name-nondirectory
     (or (executable-find%
          "xargs"
          (lambda (xargs)
            (let ((x (shell-command* "echo xxx"
                       "&& echo zzz"
                       "|xargs -0")))
              (and (zerop (car x))
                   (string-match "^zzz" (cdr x))))))
         (and (cc*-cc) (cc*-make-xargs-bin))))
    "The name of xargs executable."))

;; end of xargs

;;;
;; #include
;;;

(defun cc*-include-check (&optional remote)
  "Return a list of system cc include path."
  (let ((rc (if remote
                (shell-command* "ssh"
                  (concat (ssh-remote->user@host remote)
                          " \"echo ''|cc -v -E 2>&1 >/dev/null -\""))
              (if-platform% windows-nt
                  (shell-command* (cc*-cc))
                ;; Darwin/Linux
                (shell-command* "echo ''|" (cc*-cc)
                                " -v -E 2>&1 >/dev/null -")))))
    (when (zerop (car rc))
      (let ((pre (cdr rc)) (inc nil) (beg nil))
        (if-platform% windows-nt
            (dolist (x (var->paths
                        (car
                         (nreverse (split-string* pre "\n" t "[ \"]*"))))
                       (nreverse inc))
              (setq inc (cons (posix-path x) inc)))
          ;; Darwin/Linux
          (catch 'br
            (dolist (x (split-string* pre "\n" t "[ \t\n]"))
              (when (and beg (string-match "End of search list\\." x))
                (throw 'br (nreverse inc)))
              (when beg (setq inc (cons x inc)))
              (when (and (null beg)
                         (string-match
                          "#include <\\.\\.\\.> search starts here:" x))
                (setq beg t)))))))))

(defun cc*-system-include-read (file &optional remote)
  (or (read-sexp-from-file file)
      (let ((xs nil))
        (dolist (x (cc*-include-check remote) (nreverse xs))
          (let ((x1 (if remote (concat remote x) x)))
            (and (file-exists-p x1)
                 (setq xs (cons x1 xs))))))))

(defun cc*-system-include-file (&optional remote)
  (let* ((ss (if remote
                 (intern (mapconcat #'identity
                                    (ssh-remote->ids remote)
                                    "-"))
               'native))
         (fs (format "%s-%s.el" (v-home% ".exec/cc-inc") ss)))
    (cons ss fs)))

(defalias 'cc*-system-include
  (let ((dx nil))
    (lambda (&optional op remote)
      (let* ((ssfs (cc*-system-include-file remote))
             (ss (car ssfs)) (fs (cdr ssfs)))
        (cond ((eq op :read)
               (let ((r (cc*-system-include-read fs remote)))
                 (plist-get (setq dx (plist-put dx ss r)) ss)))
              ((eq op :save) (save-sexp-to-file (plist-get dx ss) fs))
              (t (plist-get dx ss))))))
  "Return a list of system include directories.\n
Load \\=`cc*-system-include\\=' from file when CACHED is t,
otherwise check cc include on the fly.
The REMOTE argument from \\=`ssh-remote-p\\='.")

(eval-when-compile
  (defmacro when-fn-ff-find-other-file% (&rest body)
    (if-fn% ff-find-other-file find-file
            `(progn% ,@body)
      `(comment ,@body))))

(when-fn-ff-find-other-file%
 (defun cc*-find-include-file (&optional in-other-window)
   "Find C include file in \\=`cc*-system-include\\=' or specified directory."
   (interactive "P")
   (setq% cc-search-directories
          (let ((file (buffer-file-name (current-buffer))))
            (nconc (when (stringp file)
                     (let ((pwd (file-name-directory file)))
                       (list (string-trim> pwd "/")
                             (string-trim> (path- pwd) "/"))))
                   (cc*-system-include :read (ssh-remote-p file))))
          find-file)
   (when-fn% xref-push-marker-stack xref
     (autoload 'xref-push-marker-stack "xref")
     (xref-push-marker-stack))
   (ff-find-other-file in-other-window nil)))

;; end of #include

;;;
;; #define
;;;

(when-platform% windows-nt
  (defun cc*-make-macro-dump-bin (&optional options)
    "Make cc-dump-macro.exe for printing predefined macros."
    (let ((c (v-home% ".exec/cc-dump-macro.c"))
          (exe (v-home% ".exec/cc-dump-macro.exe")))
      (unless (file-exists-p c)
        (copy-file (emacs-home% "config/sample-cc-dump-macro.c") c))
      (let ((cmd (shell-command* (cc*-cc)
                   (concat " -nologo"
                           " " options
                           " " c
                           " -Fo" temporary-file-directory
                           " -Fe" exe))))
        (when (zerop (car cmd))
          (file-name-nondirectory exe))))))


(defun cc*-dump-predefined-macros (&optional options)
  "Dump predefined macros."
  (interactive "sInput C compiler's options: ")
  (let* ((remote (ssh-remote-p (buffer-file-name (current-buffer))))
         (cc (if remote "cc" (cc*-cc)))
         (opts  (format "%s -dM -E -" options))
         (rc (cond (remote (fluid-let (shell-file-name "sh")
                             (shell-command* "ssh"
                               (ssh-remote->user@host remote)
                               cc opts)))
                   (t (if-platform% windows-nt
                          (cc*-make-macro-dump-bin options)
                        (shell-command* cc opts))))))
    (with-current-buffer
        (switch-to-buffer
         (format "*Macro Predefined%s*"
                 (if remote
                     (concat "@" (ssh-remote->user@host remote))
                   "")))
      (view-mode -1)
      (erase-buffer)
      (insert (if (zerop (car rc))
                  (if (> (length (cdr rc)) 0)
                      (cdr rc)
                    "/* C preprocessor no output! */")
                (cdr rc)))
      (c-mode)
      (goto-char (point-min))
      (view-mode 1))))

;; end of #define

;;;
;; `tags'
;;;

(defun cc*-make-tags (&optional renew)
  "Make system C tags."
  (let ((file (concat (tags-spec->% :root) "os.TAGS"))
        (opt "--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq"))
    (cond (renew (let ((inc (cc*-system-include :read)))
                   (make-c-tags (car inc) file opt nil nil t)
                   (dolist (p (cdr inc) file)
                     (make-c-tags p file opt))))
          (t file))))

;; end of `cc*-make-tags'

;;;
;; `cc-styles'
;;;

(defun cc*-style-arglist-cont-nonempty (langem)
  (let ((col (save-excursion
               (goto-char (cdr langem))
               (current-column))))
    (cond ((= col 0) 'c-basic-offset)
          (t 'c-lineup-arglist))))

(defconst +cc*-style-nginx+
  `("nginx"
    (c-basic-offset . 4)
    (c-comment-only-line-offset . 0)
    (c-backslash-max-column . 78)
    (c-backslash-column . 77)
    (c-offsets-alist
     (statement-block-intro . +)
     (substatement-open . 0)
     (substatement-label . 0)
     (label . 0)
     (statement-cont . +)
     (inline-open . 0)
     (brace-list-intro
      first
      ,(when-fn% c-lineup-2nd-brace-entry-in-arglist cc-align
         #'c-lineup-2nd-brace-entry-in-arglist)
      ,(when-fn% c-lineup-class-decl-init-+ cc-align
         #'c-lineup-class-decl-init-+)
      +)
     (arglist-cont-nonempty
      .
      #'cc*-style-arglist-cont-nonempty)))
  "nginx style for \\=`cc-styles\\='.
https://nginx.org/en/docs/dev/development_guide.html#code_style")

(defun cc*-style-align-entire (begin end &optional n)
  "Align the selected region as if it were one alignment section.\n
BEGIN and END mark the extent of the region.
N specify the number of spaces when align."
  (interactive "r\nP")
  (when-fn% align-entire align
    (when-var% align-default-spacing align
      (fluid-let (align-default-spacing (or n 2))
        (align-entire begin end)))))

;; end of `cc-styles'

;;;
;; format
;;;

(defun cc*-format-buffer-shell (src)
  (let ((dst (make-temp-file "cc-fmt-dst-" nil ".c")))
    (let ((x (shell-command* "cat <" src "|clang-format >" dst)))
      (and (zerop (car x)) dst))))

(defun cc*-format-buffer ()
  "Format the current buffer via clang-format."
  (interactive)
  (shell-format-buffer `(c-mode c-ts-mode)
    (when-feature% eglot
      (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
        (catch 'br
          (call-interactively #'eglot-format-buffer)
          (throw 'br t))))
    (make-temp-file "cc-fmt-src-" nil ".c")
    #'cc*-format-buffer-shell))

;; end of format

;;;
;; `cmacexp'
;;;

(eval-when-compile
  (defmacro when-fn-c-macro-expand% (&rest body)
    (declare (indent 0))
    (if-fn% c-macro-expand cmacexp
            `(progn% ,@body)
      `(comment ,@body))))

(when-fn-c-macro-expand%
  (defun cc*-macro-expand ()
    "Macro expanding current buffer."
    (let ((remote (ssh-remote-p (buffer-file-name (current-buffer)))))
      (setq% c-macro-prompt-flag t cmacexp)
      (setq% c-macro-buffer-name
              (if remote
                  (format "*Macro Expanded@%s*"
                          (ssh-remote->user@host remote))
                "*Macro Expanded*")
              cmacexp)
      (setq% c-macro-preprocessor
              (if remote
                  (format "ssh %s %s"
                          (ssh-remote->user@host remote)
                          "cc -E -o - -")
                (if-platform% windows-nt
                    ;; cl.exe can't compile on the fly without xargs
                    (let ((tmp (make-temp-file "cc-m-" nil ".c")))
                      (format "%s -0 > %s && %s && cl -E %s"
                              +cc*-xargs-bin+ tmp
                              (cc*-cc) tmp))
                  (format "%s -E -o - -" (cc*-cc))))
              cmacexp))))

(when-fn-c-macro-expand%
  (defadvice c-macro-expand
      (around c-macro-expand-around first compile disable)
    (cc*-macro-expand)
    ad-do-it))

;; end of `cmacexp'

;;;
;; `man'
;;;

(defun on-man-init! ()
  "On \\=`man\\=' initialization."
  ;; fix cannot find include path on Darwin in `Man-mode'
  (setq% Man-header-file-path (cc*-system-include :read) man))

;; end of `man'

;;;
;; keys
;;;

(defun cc*-define-keys (keymap)
  ;; dump predefined macros
  (define-key keymap "#" #'cc*-dump-predefined-macros)
  ;; raw newline
  (define-key keymap (kbd% "RET") #'newline*)
  ;; align style
  (define-key keymap "|" #'cc*-style-align-entire)
  ;; format buffer
  (define-key keymap (kbd% "C-c M-c f") #'cc*-format-buffer)
  (when-fn-ff-find-other-file%
   (define-key keymap "fi" #'cc*-find-include-file))
  (when-fn-c-macro-expand%
   (define-key keymap "" #'c-macro-expand)))

;; end of keys

;;;
;; `cc-mode'
;;;

(defun on-cc-mode-init! ()
  "On \\=`cc-mode\\=' initialization."
  ;; add styles
  ;; (c-add-style (car +cc*-style-nginx+) (cdr +cc*-style-nginx+))
  (when-fn-c-macro-expand%
    ;; [C-c C-e] `c-macro-expand'
    (setq% c-macro-prompt-flag t cmacexp)
    (ad-enable-advice #'c-macro-expand 'around "c-macro-expand-around")
    (ad-activate #'c-macro-expand t))
  ;; indent line or region
  (when-fn% c-indent-line-or-region cc-cmds
    (define-key% c-mode-map (kbd "TAB") #'c-indent-line-or-region))
  ;; `subword-mode'
  (if-fn% subword-mode subword
          (define-key% c-mode-map "" #'subword-mode)
    (define-key% c-mode-map "" #'c-subword-mode))
  (when (boundp 'c-mode-map)
    (cc*-define-keys c-mode-map))
  (when (boundp 'c++-mode-map)
    (cc*-define-keys c++-mode-map)))

;; end of `cc-mode'

;;;
;; `c-ts-mode'
;;;

(when-feature-treesit%
  (defun on-c-ts-mode-init! ()
    "On \\=`c-ts-mode\\=' initialization."
    (when-fn-c-macro-expand%
      (setq% c-macro-prompt-flag t cmacexp)
      (ad-enable-advice #'c-macro-expand 'around "c-macro-expand-around")
      (ad-activate #'c-macro-expand t))
    (when (boundp 'c-ts-mode-map)
      (cc*-define-keys c-ts-mode-map))
    (when (boundp 'c++-ts-mode-map)
      (cc*-define-keys c++-ts-mode-map))))

;; end of `c-ts-mode'

(provide 'cc)

;; end of cc.el
