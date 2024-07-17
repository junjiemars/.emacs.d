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

(declare-function make-c-tags (v-home%> "config/tags"))
(autoload 'make-c-tags (v-home%> "config/tags")
  "Make C tags.")

;; end of require

;;;
;; msvc host environment
;;;

(when-platform% 'windows-nt
  (defun cc*-check-vcvarsall-bat ()
    "Return the path of vcvarsall.bat if which exists."
    (let* ((pfroot (posix-path (getenv "PROGRAMFILES")))
           (vsroot (concat pfroot " (x86)/Microsoft Visual Studio/"))
           (vswhere (concat vsroot "Installer/vswhere.exe")))
      (when (file-exists-p vswhere)
        (posix-path
         (or (let* ((cmd (shell-command* (shell-quote-argument vswhere)
                           "-nologo -latest -property installationPath"))
                    (bat (and (zerop (car cmd))
                              (concat (string-trim> (cdr cmd))
                                      "/VC/Auxiliary/Build/vcvarsall.bat"))))
               (when (file-exists-p bat) bat))
             (let* ((ver (car (directory-files
                               vsroot
                               t "[0-9]+" #'string-greaterp)))
                    (bat (concat
                          ver
                          "/BuildTools/VC/Auxiliary/Build/vcvarsall.bat")))
               (when (file-exists-p bat) bat))))))))

(when-platform% 'windows-nt
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
                 "set AS=ml" (if (string-match "[_a-zA-Z]*64" arch) "64" "")
                 "\n"
                 "\n"
                 "popd\n"
                 "echo \"%INCLUDE%\"\n")
         (v-home% ".exec/cc-env.bat"))))))

;; msvc host environment

;;;
;; CC
;;;

(defalias 'cc*-cc
  (lexical-let%
      ((b (let ((cx (if-platform% 'windows-nt
                        (progn%
                         (unless (executable-find% "cc-env.bat")
                           (cc*-make-env-bat))
                         '("cc-env.bat" "cl" "gcc"))
                      '("cc" "gcc" "clang")))
                (o (make-temp-file "cc-c-" nil
                                   (if-platform% 'windows-nt
                                       ".exe"
                                     ".out")))
                (i (save-str-to-file
                    (concat
                     "int main(void) {\n"
                     "  return 0;\n"
                     "}")
                    (make-temp-file "cc-s-" nil ".c"))))
            (catch 'br
              (dolist* (cc cx)
                (when (zerop
                       (car
                        (shell-command*
                            (format (if-platform% 'windows-nt
                                        (if (string= "cc-env.bat" cc)
                                            (concat "%s %s -Fe%s -Fo")
                                          "%s %s -o%s")
                                      "%s %s -o%s")
                                    (if-platform% 'windows-nt
                                        (if (string= "cc-env.bat" cc)
                                            "cc-env.bat && cl"
                                          cc)
                                      cc)
                                    i o))))
                  (throw 'br cc)))))))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "The name of C compiler executable.")

;; end of CC

;;;
;; xargs
;;;

(when-platform% 'windows-nt
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
        (let ((cmd (shell-command* cc)))
          (when (zerop (car cmd))
            (file-name-nondirectory exe)))))))

(when-platform% 'windows-nt
  (defconst +cc*-xargs-bin+
    (eval-when-compile
      (file-name-nondirectory%
       (or (executable-find%
            "xargs"
            (lambda (xargs)
              (let ((x (shell-command* "echo xxx"
                         "&& echo zzz"
                         "|xargs -0")))
                (and (zerop (car x))
                     (string-match "^zzz" (cdr x))))))
           (and (cc*-cc)
                (cc*-make-xargs-bin)))))
    "The name of xargs executable."))

;; end of xargs

;;;
;; #include
;;;

(defun cc*-check-include (&optional remote)
  "Return a list of system cc include path."
  (let ((cmd (if remote
                 (when% (executable-find% "ssh")
                   (shell-command* "ssh"
                     (concat
                      (ssh-remote->user@host remote)
                      " \"echo ''|cc -v -E 2>&1 >/dev/null -\"")))
               (if-platform% 'windows-nt
                   ;; Windows: msmvc
                   (shell-command* (cc*-cc))
                 ;; Darwin/Linux: clang or gcc
                 (shell-command*
                     (concat "echo ''|"
                             (cc*-cc)
                             " -v -E 2>&1 >/dev/null -")))))
        (parser (lambda (pre)
                  (if-platform% 'windows-nt
                      ;; Windows: msvc
                      (mapcar
                       (lambda (x) (posix-path x))
                       (var->paths
                        (car (nreverse
                              (split-string* pre "\n" t "[ \"]*")))))
                    ;; Darwin/Linux: clang or gcc
                    (cdr
                     (take-while
                      (lambda (p)
                        (string-match "End of search list\\." p))
                      (drop-while
                       (lambda (p)
                         (not
                          (string-match
                           "#include <\\.\\.\\.> search starts here:"
                           p)))
                       (split-string* pre "\n" t "[ \t\n]"))))))))
    (when (zerop (car cmd))
      (funcall parser (cdr cmd)))))


(defalias 'cc*-system-include
  (lexical-let% ((dx nil))
    (lambda (&optional cached remote)
      (let* ((ss (if remote
                     (intern
                      (mapconcat #'identity
                                 (ssh-remote->ids remote)
                                 "-"))
                   'native))
             (fs (concat (v-home% ".exec/cc-inc-")
                         (symbol-name ss) ".el"))
             (d))
        (or (and cached (plist-get dx ss))

            (and cached (file-exists-p fs)
                 (plist-get
                  (setq dx (plist-put dx ss
                                      (read-sexp-from-file fs)))
                  ss))

            (and (setq d (mapcar (if remote
                                     (lambda (x)
                                       (concat remote x))
                                   #'identity)
                                 (cc*-check-include remote)))
                 (consp d) (save-sexp-to-file d fs)
                 (plist-get (setq dx (plist-put dx ss d)) ss))))))
  "Return a list of system include directories.\n
Load \\=`cc*-system-include\\=' from file when CACHED is t,
otherwise check cc include on the fly.
The REMOTE argument from \\=`ssh-remote-p\\='.")

(defalias 'cc*-extra-include
  (lexical-let% ((dx nil)
                 (fs (v-home% ".exec/cc-extra-inc.el")))
    (lambda (cached &rest dir)
      (or (and cached dx)

          (and cached (file-exists-p fs)
               (setq dx (read-sexp-from-file fs)))

          (and (consp dir)
               (save-sexp-to-file
                (setq dx
                      (append dx
                              (mapcar (lambda (x)
                                        (expand-file-name
                                         (string-trim> x "/")))
                                      dir)))
                fs)
               dx))))
  "Return a list of extra include directories.")


;; (defun cc*-include-p (file)
;;   "If FILE in \\=`cc*-system-include\\=' yield non-nil, else nil."
;;   (when (stringp file)
;;     (let ((remote (ssh-remote-p file)))
;;       (file-in-dirs-p (file-name-directory file)
;;                       (if remote
;;                           (cc*-system-include t remote)
;;                         (append (cc*-system-include t)
;;                                 (cc*-extra-include t)))))))


;; (defun cc*-view-include (buffer)
;;   "View cc's BUFFER in \\=`view-mode\\='."
;;   (when (and (bufferp buffer)
;;              (let ((m (buffer-local-value 'major-mode buffer)))
;;                (or (eq 'c-mode m)
;;                    (eq 'c++-mode m)))
;;              (cc*-include-p (substring-no-properties
;;                              (buffer-file-name buffer))))
;;     (with-current-buffer buffer (view-mode 1))))

(defmacro when-fn-ff-find-other-file% (&rest body)
  (when-fn% 'ff-find-other-file 'find-file
    `(progn% ,@body)))

(when-fn-ff-find-other-file%
 (defun cc*-find-include-file (&optional in-other-window)
   "Find C include file in \\=`cc*-system-include\\=' or specified directory. "
   (interactive "P")
   (setq% cc-search-directories
          (let ((file (buffer-file-name (current-buffer))))
            (delq nil
                  (append (list
                           (when (stringp file)
                             (string-trim>
                              (file-name-directory file) "/")))
                          (cc*-system-include t (ssh-remote-p file))
                          (cc*-extra-include t))))
          'find-file)
   (when-fn% 'xref-push-marker-stack 'xref
     (autoload 'xref-push-marker-stack "xref")
     (xref-push-marker-stack))
   (ff-find-other-file in-other-window nil)))

;; end of #include

;;;
;; #define
;;;

(when-platform% 'windows-nt
  (defun cc*-make-macro-dump-bin (&optional options)
    "Make cc-dmacro.exe for printing predefined macros."
    (let ((c (v-home% ".exec/cc-dmacro.c"))
          (exe (v-home% ".exec/cc-dmacro.exe")))
      (unless (file-exists-p c)
        (copy-file (emacs-home* "config/sample-cc-dmacro.c") c))
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
         (rc (cond (remote
                    (fluid-let (shell-file-name "sh")
                      (shell-command* "ssh"
                        (ssh-remote->user@host remote)
                        cc opts)))
                   (t (if-platform% 'windows-nt
                          (cc*-make-macro-dump-bin options)
                        (shell-command* cc opts))))))
    (with-current-buffer
        (switch-to-buffer
         (concat "*Macro Predefined"
                 (if remote
                     (concat "@" (ssh-remote->user@host remote)
                             "*")
                   "*")))
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

(defalias 'cc*-make-tags
  (lexical-let%
      ((b (concat (tags-spec->% :root) "os.TAGS"))
       (option "--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq")
       (skip (concat "cpp\\|c\\+\\+"
                     "\\|/python.*?/"
                     "\\|/php.*?/"
                     "\\|/ruby.*?/"
                     "\\|/swift/")))
    (lambda (&optional op)
      (cond ((eq op :new)
             (let ((inc (cc*-system-include))
                   (filter (lambda (_ a)
                             (not (string-match skip a)))))
               (make-c-tags (car inc) b option nil filter)
               (dolist* (p (cdr inc) b)
                 (make-c-tags p b option nil filter))))
            (t (inhibit-file-name-handler
                 (and b (file-exists-p b) b))))))
  "Make system C tags.")

;; end of `cc*-make-tags'

;;;
;; `cc-styles'
;;;

(defvar cc*-style-nginx
  (eval-when-compile
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
        ,(when-fn% 'c-lineup-2nd-brace-entry-in-arglist
             'cc-align
           #'c-lineup-2nd-brace-entry-in-arglist)
        ,(when-fn% 'c-lineup-class-decl-init-+
             'cc-align
           #'c-lineup-class-decl-init-+)
        +)
       (arglist-cont-nonempty
        .
        ,(lambda (langem)
           (let ((col (save-excursion
                        (goto-char (cdr langem))
                        (current-column))))
             (cond ((= col 0) 'c-basic-offset)
                   (t 'c-lineup-arglist))))))))
  "nginx style for \\=`cc-styles\\='.
https://nginx.org/en/docs/dev/development_guide.html#code_style")

(defun cc*-style-align-entire (begin end &optional n)
  "Align the selected region as if it were one alignment section.\n
BEGIN and END mark the extent of the region.
N specify the number of spaces when align."
  (interactive "r\nP")
  (when-fn% 'align-entire 'align
    (when-var% align-default-spacing 'align
      (fluid-let (align-default-spacing (or n 2))
        (align-entire begin end)))))

;; end of `cc-styles'

;;;
;; `cc-mode'
;;;

(defun on-cc-mode-init! ()
  "On \\=`cc-mode\\=' initialization."
  ;; load styles
  (c-add-style (car cc*-style-nginx) (cdr cc*-style-nginx))
  ;; keymap:
  ;; find include file
  (when-fn-ff-find-other-file%
   (when-var% c-mode-map 'cc-mode
     (define-key% c-mode-map (kbd "C-c f i")
                  #'cc*-find-include-file))
   ;; for c++, add include via `cc*-extra-include'
   (when-var% c++mode-map 'cc-mode
     (define-key% c++-mode-map (kbd "C-c f i")
                  #'cc*-find-include-file)))
  ;; indent line or region
  (when-fn% 'c-indent-line-or-region 'cc-cmds
    (define-key% c-mode-map
                 (kbd "TAB") #'c-indent-line-or-region))
  ;; dump predefined macros
  (define-key% c-mode-map (kbd "C-c #")
               #'cc*-dump-predefined-macros)
  ;; raw newline
  (define-key% c-mode-map (kbd "RET") #'newline*)
  ;; align style
  (define-key% c-mode-map (kbd "C-c |") #'cc*-style-align-entire)
  ;; `subword-mode'
  (define-key% c-mode-map (kbd "C-c C-w")
               (if-fn% 'subword-mode 'subword
                       #'subword-mode
                 #'c-subword-mode)))

;; end of `cc-mode'

;;;
;; `cmacexp'
;;;

(defmacro when-fn-c-macro-expand% (&rest body)
  (declare (indent 0))
  (when-fn% 'c-macro-expand 'cmacexp
    `(progn% ,@body)))

(when-fn-c-macro-expand%
  (defun cc*-macro-expand ()
    "Macro expanding current buffer."
    (let ((remote (ssh-remote-p (buffer-file-name (current-buffer)))))
      (setq% c-macro-prompt-flag t 'cmacexp)
      (setq% c-macro-buffer-name
             (if remote
                 (format "*Macro Expanded@%s*"
                         (ssh-remote->user@host remote))
               "*Macro Expanded*")
             'cmacexp)
      (setq% c-macro-preprocessor
             (if remote
                 (format "ssh %s %s"
                         (ssh-remote->user@host remote)
                         "cc -E -o - -")
               (if-platform% 'windows-nt
                   ;; cl.exe can't compile on the fly without xargs
                   (let ((tmp (make-temp-file "cc-m-" nil ".c")))
                     (format "%s -0 > %s && %s && cl -E %s"
                             +cc*-xargs-bin+ tmp
                             (cc*-cc) tmp))
                 (format "%s -E -o - -" (cc*-cc))))
             'cmacexp))))

(when-fn-c-macro-expand%
  (defadvice c-macro-expand
      (around c-macro-expand-around first compile disable)
    "Expand C macros in the region, using the C preprocessor."
    (cc*-macro-expand)
    ad-do-it))

(defun on-cmacexp-init! ()
  "On \\=`cmacexp\\=' initialization."
  (when-fn-c-macro-expand%
    ;; [C-c C-e] `c-macro-expand' in `cc-mode'
    (setq% c-macro-prompt-flag t 'cmacexp)
    (ad-enable-advice #'c-macro-expand
                      'around "c-macro-expand-around")
    (ad-activate #'c-macro-expand t)))

;; end of `cmacexp'

;;;
;; `man'
;;;

(defun on-man-init! ()
  "On \\=`man\\=' initialization."
  ;; fix cannot find include path on Darwin in `Man-mode'
  (when-var% manual-program 'man
    (when% (executable-find% manual-program)
      (setq% Man-header-file-path (cc*-system-include t) 'man))))

;; end of `man'

(provide 'cc)

;; end of cc.el
