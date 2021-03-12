;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-cc-autoload.el
;;;;


;; msvc host environment

(when-platform% 'windows-nt

  (defun check-vcvarsall-bat ()
    "Return the path of vcvarsall.bat if which exists."
    (let* ((pfroot (posix-path (getenv "PROGRAMFILES")))
           (vsroot (concat pfroot " (x86)/Microsoft Visual Studio/"))
           (vswhere (concat vsroot "Installer/vswhere.exe")))
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
             (when (file-exists-p bat) bat)))))))


(when-platform% 'windows-nt

  (defun make-cc-env-bat ()
    "Make cc-env.bat in `exec-path'."
    (let ((vcvarsall (check-vcvarsall-bat))
          (arch (downcase (getenv "PROCESSOR_ARCHITECTURE"))))
      (when vcvarsall
        (save-str-to-file
         (concat "@echo off\n"
                 (concat "rem generated by More Reasonable Emacs"
                         (more-reasonable-emacs) "\n\n")
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


(defconst +cc*-compiler-bin+
  (file-name-nondirectory%
   (executable-find%
    (if-platform% 'windows-nt
        (or (let ((cc (executable-find% "cc-env.bat")))
              (file-name-nondirectory cc))
            (make-cc-env-bat))
      "cc")
    (lambda (cc)
      (let ((c (concat temporary-file-directory "cc-bin.c")))
        (save-str-to-file (concat
                           "int main(int argc, char **argv) {\n"
                           "  return 0;\n"
                           "}")
                          c)
        (let ((x (shell-command* cc
                   (concat (if-platform% 'windows-nt
                               (concat
                                " && cl -nologo"
                                " " c
                                " -Fo" temporary-file-directory
                                " -Fe")
                             (concat c " -o"))
                           (concat temporary-file-directory "cc-bin.out")))))
          (zerop (car x)))))))
  "The name of C compiler executable.")


(when-platform% 'windows-nt

  (defun make-xargs-bin ()
    "Make a GNU's xargs alternation in `exec-path'."
    (let* ((c (concat temporary-file-directory "xargs.c"))
           (exe (v-home% ".exec/xargs.exe"))
           (cc (concat +cc*-compiler-bin+
                       " && cl -nologo -DNDEBUG=1 -O2 -utf-8"
                       " " c
                       " -Fo" temporary-file-directory
                       " -Fe" exe
                       " -link -release")))
      (when (save-str-to-file
             (eval-when-compile
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
                       "}\n"))
             c)
        (let ((cmd (shell-command* cc)))
          (when (zerop (car cmd))
            (file-name-nondirectory exe)))))))


(when-platform% 'windows-nt

  (defconst +cc*-xargs-bin+
    (file-name-nondirectory%
     (or (executable-find%
          "xargs"
          (lambda (xargs)
            (let ((x (shell-command* "echo xxx"
                       "&& echo zzz"
                       "|xargs -0")))
              (and (zerop (car x))
                   (string-match "^zzz" (cdr x))))))
         (and +cc*-compiler-bin+
              (make-xargs-bin))))
    "The name of xargs executable."))


(when-platform% 'windows-nt

  (defun make-cc-dmacro-bin (&optional options)
    "Make cc-dmacro.exe for printing predefined macros."
    (let* ((c (concat temporary-file-directory "cc-dmacro.c"))
           (exe (v-home% ".exec/cc-dmacro.exe")))
      (save-str-to-file
       (eval-when-compile
         (concat "#include <stdio.h>\n"
                 "#define _STR2_(x) #x\n"
                 "#define _STR1_(x) _STR2_(x)\n"
                 "#define _POUT_(x) \"#define \" #x \" \" _STR1_(x) \"\\n\"\n"
                 "int main(int argc, char **argv) {\n"
                 "#if defined(__STDC__)\n" ;; -Za specified
                 "   printf(_POUT_(__STDC__));\n"
                 "#endif\n"
                 "#if defined( __STDC_HOSTED__)\n"
                 "   printf(_POUT_(__STDC_HOSTED__));\n"
                 "#endif\n"
                 "#if defined(__STDCPP_THREADS__)\n"
                 "   printf(_POUT_(__STDCPP_THREADS__));\n"
                 "#endif\n"
                 "#if defined(_WIN32)\n"
                 "  printf(_POUT_(_WIN32));\n"
                 "#endif\n"
                 "#if defined(_WIN64)\n"
                 "   printf(_POUT_(_WIN64));\n"
                 "#endif\n"
                 "#if defined(_WINRT_DLL)\n"
                 "  printf(_POUT_(_WINRT_DLL));\n"
                 "#endif\n"
                 "#if defined(__ATOM__)\n"
                 "  printf(_POUT_(__ATOM__));\n"
                 "#endif\n"
                 "#if defined(__AVX__)\n"
                 "  printf(_POUT_(__AVX__));\n"
                 "#endif\n"
                 "#if defined(__AVX2__)\n"
                 "  printf(_POUT_(__AVX2__));\n"
                 "#endif\n"
                 "#if defined(__AVX512BW__)\n"
                 "  printf(_POUT_(__AVX512BW__));\n"
                 "#endif\n"
                 "#if defined(__AVX512CD__)\n"
                 "  printf(_POUT_(__AVX512CD__));\n"
                 "#endif\n"
                 "#if defined(__AVX512DQ__)\n"
                 "  printf(_POUT_(__AVX512DQ__));\n"
                 "#endif\n"
                 "#if defined(__AVX512F__)\n"
                 "  printf(_POUT_(__AVX512F__));\n"
                 "#endif\n"
                 "#if defined(__AVX512VL__)\n"
                 "  printf(_POUT_(__AVX512VL__));\n"
                 "#endif\n"
                 "#if defined(_CHAR_UNSIGNED)\n"
                 "  printf(_POUT_(_CHAR_UNSIGNED));\n" ;; -J specified
                 "#endif\n"
                 "#if defined(_CONTROL_FLOW_GUARD)\n"
                 "  printf(_POUT_(_CONTROL_FLOW_GUARD));\n"
                 "#endif\n"
                 "#if defined(__COUNTER__)\n"
                 "  printf(_POUT_(__COUNTER__));\n"
                 "#endif\n"
                 "#if defined(__cplusplus_winrt)\n"
                 "  printf(_POUT_(__cplusplus_winrt));\n"
                 "#endif\n"
                 "#if defined(_CPPUNWIND)\n"
                 "  printf(_POUT_(_CPPUNWIND));\n"
                 "#endif\n"
                 "#if defined(_DEBUG)\n"
                 "  printf(_POUT_(_DEBUG));\n"
                 "#endif\n"
                 "#if define(_DLL)\n"
                 "  printf(_POUT_(_DLL));\n"
                 "#endif\n"
                 "#if defined(_INTEGRAL_MAX_BITS)\n"
                 "  printf(_POUT_(_INTEGRAL_MAX_BITS));\n"
                 "#endif\n"
                 "#if defined(_ISO_VOLATILE )\n"
                 "  printf(_POUT_(_ISO_VOLATILE));\n"
                 "#endif\n"
                 "#if defined(_KERNEL_MODE)\n"
                 "  printf(_POUT_(_KERNEL_MODE));\n"
                 "#endif\n"
                 "#if defined(_M_AMD64)\n"
                 "  printf(_POUT_(_M_AMD64));\n"
                 "#endif\n"
                 "#if defined(_M_FP_EXCEPT)\n"
                 "  printf(_POUT_(_M_FP_EXCEPT));\n"
                 "#endif\n"
                 "#if defined(_M_FP_FAST)\n"
                 "  printf(_POUT_(_M_FP_FAST));\n"
                 "#endif\n"
                 "#if defined(_M_FP_PRECISE)\n"
                 "  printf(_POUT_(_M_FP_PRECISE));\n"
                 "#endif\n"
                 "#if defined(_M_FP_STRICT)\n"
                 "  printf(_POUT_(_M_FP_STRICT));\n"
                 "#endif\n"
                 "#if defined(_M_IX86)\n"
                 "  printf(_POUT_(_M_IX86));\n"
                 "#endif\n"
                 "#if defined(_M_IX86_FP)\n"
                 "  printf(_POUT_(_M_IX86_FP));\n"
                 "#endif\n"
                 "#if defined(_M_X64)\n"
                 "  printf(_POUT_(_M_X64));\n"
                 "#endif\n"
                 "#if defined(_MSC_BUILD)\n"
                 "  printf(_POUT_(_MSC_BUILD));\n"
                 "#endif\n"
                 "#if defined(_MSC_EXTENSIONS)\n"
                 "  printf(_POUT_(_MSC_EXTENSIONS));\n"
                 "#endif\n"
                 "#if defined(_MSC_FULL_VER)\n"
                 "  printf(_POUT_(_MSC_FULL_VER));\n"
                 "#endif\n"
                 "#if defined(_MSC_VER)\n"
                 "  printf(_POUT_(_MSC_VER));\n"
                 "#endif\n"
                 "#if defined(_MSVC_LANG)\n"
                 "  printf(_POUT_(_MSVC_LANG));\n"
                 "#endif\n"
                 "#if defined(__MSVC_RUNTIME_CHECKS)\n"
                 "  printf(_POUT_(__MSVC_RUNTIME_CHECKS));\n"
                 "#endif\n"
                 "#if defined(_MSVC_TRADITIONAL)\n"
                 "  printf(_POUT_(_MSVC_TRADITIONAL));\n"
                 "#endif\n"
                 "#if defined(_MT)\n"
                 "  printf(_POUT_(_MT));\n"
                 "#endif\n"
                 "#if defined(_NATIVE_WCHAR_T_DEFINED)\n"
                 "  printf(_POUT_(_NATIVE_WCHAR_T_DEFINED));\n"
                 "#endif\n"
                 "#if defined(_OPENMP)\n"
                 "  printf(_POUT_(_OPENMP));\n"
                 "#endif\n"
                 "#if defined(_PREFAST_)\n"
                 "  printf(_POUT_(_PREFAST_));\n"
                 "#endif\n"
                 "#if defined(_VC_NODEFAULTLIB)\n"
                 "  printf(_POUT_(_VC_NODEFAULTLIB));\n"
                 "#endif\n"
                 "#if defined(_WCHAR_T_DEFINED)\n"
                 "  printf(_POUT_(_WCHAR_T_DEFINED));\n"
                 "#endif\n"
                 ""
                 "}"))
       c)
      (let ((cmd (shell-command* +cc*-compiler-bin+
                   (concat " && cl -nologo"
                           " " options
                           " " c
                           " -Fo" temporary-file-directory
                           " -Fe" exe))))
        (when (zerop (car cmd))
          (file-name-nondirectory exe))))))


(defun cc*-check-include (&optional remote)
  "Return a list of system cc include path."
  (let ((cmd (if remote
                 (when% (executable-find% "ssh")
                   (shell-command* "ssh"
                     (concat (remote-norm->user@host remote)
                             " \"echo '' | cc -v -E 2>&1 >/dev/null -\"")))
               (if-platform% 'windows-nt
                   ;; Windows: msmvc
                   (shell-command* +cc*-compiler-bin+)
                 ;; Darwin/Linux: clang or gcc
                 (shell-command* (concat "echo '' | "
                                         +cc*-compiler-bin+
                                         " -v -E 2>&1 >/dev/null -")))))
        (parser (lambda (preprocessed)
                  (take-while
                   (lambda (p)
                     (string-match "End of search list." p))
                   (drop-while
                    (lambda (p)
                      (string-match "#include <...> search starts here:" p))
                    (split-string* preprocessed "\n" t "[ \t\n]"))))))
    (when (zerop (car cmd))
      (if remote
          ;; Unix-like
          (funcall parser (cdr cmd))
        (if-platform% 'windows-nt
            ;; Windows: msvc
            (mapcar (lambda (x) (posix-path x))
                    (var->paths
                     (car (nreverse
                           (split-string* (cdr cmd) "\n" t "[ \"]*")))))
          ;; Darwin/Linux: clang or gcc
          (let ((inc (funcall parser (cdr cmd))))
            (if-platform% 'darwin
                (mapcar (lambda (x)
                          (file-truename
                           (string-trim> x " (framework directory)")))
                        inc)
              inc)))))))


;; cc system include

(defun cc*-system-include (&optional cached remote)
  "Return a list of system include directories.

Load `cc*-system-include' from file when CACHED is t,
otherwise check cc include on the fly.

If specify REMOTE argument then return a list of remote system
include directories. The REMOTE argument from `remote-norm-file'."
  (let* ((rid (when remote
                (mapconcat #'identity (remote-norm-id remote) "-")))
         (c (if remote
                (v-home* (concat ".exec/.cc-inc-" rid ".el"))
              (v-home% ".exec/.cc-inc.el")))
         (cc (concat c "c"))
         (var (if remote
                  (intern (concat "cc*-system-include@" rid))
                'cc*-system-include)))
    (if (and cached (file-exists-p cc))
        (load cc)
      (let ((inc (if remote
                     (mapcar (lambda (x)
                               (concat remote x))
                             (cc*-check-include remote))
                   (cc*-check-include))))
        (set var inc)
        (when (save-sexp-to-file `(set ',var ',inc) c)
          (byte-compile-file c))))
    (symbol-value var)))


(defun cc*-extra-include (cached &rest dir)
  "Return a list of extra include directories."
  (declare (indent 1))
  (let* ((c (v-home% ".exec/.cc-extra-inc.el"))
         (cc (concat c "c"))
         (var 'cc*-extra-include)
         (d1 (when (consp dir)
               (mapcar (lambda (x)
                         (expand-file-name (string-trim> x "/")))
                       dir))))
    (if cached
        (when (or (boundp var)
                  (and (file-exists-p cc)
                       (load cc)))
          (symbol-value var))
      (prog1
          (set var d1)
        (when (save-sexp-to-file `(set ',var ',(symbol-value var)) c)
          (byte-compile-file c))))))


(defun cc*-include-p (file)
  "Return t if FILE in `cc*-system-include', otherwise nil."
  (when (stringp file)
    (let ((remote (remote-norm-file file)))
      (file-in-dirs-p (file-name-directory file)
                      (if remote
                          (cc*-system-include t remote)
                        (append (cc*-system-include t)
                                (cc*-extra-include t)))))))


(defun cc*-view-include (buffer)
  "View cc's BUFFER in `view-mode'.

When BUFFER in `c-mode' or `c++-mode' and `cc*-system-include' or
`cc*-extra-include' is t then view it in `view-mode'."
  (when (and (bufferp buffer)
             (let ((m (buffer-local-value 'major-mode buffer)))
               (or (eq 'c-mode m)
                   (eq 'c++-mode m)))
             (cc*-include-p (substring-no-properties
                             (buffer-file-name buffer))))
    (with-current-buffer buffer (view-mode 1))))


 ;; end of cc*-system-include


(defun cc*-find-include-file (&optional in-other-window)
  "Find C include file in `cc*-system-include' or specified directory. "
  (interactive "P")
  (let ((file (buffer-file-name (current-buffer))))
    (setq% cc-search-directories
           (append (list (string-trim> (file-name-directory file) "/"))
                   (cc*-system-include t (remote-norm-file file))
                   (cc*-extra-include t))
           'find-file)
    (when-fn% 'xref-push-marker-stack 'xref
      (autoload 'xref-push-marker-stack "xref.elc")
      (xref-push-marker-stack))
    (ff-find-other-file in-other-window nil)))


(defadvice c-macro-expand (around c-macro-expand-around disable)
  "Expand C macros in the region, using the C preprocessor."
  (let ((remote (remote-norm-file (buffer-file-name (current-buffer)))))
    (if remote
        ;; remote: Unix-like
        (when% (executable-find% "ssh")
          (setq% c-macro-buffer-name
                 (concat "*Macroexpansion@"
                         (remote-norm->user@host remote)
                         "*")
                 'cmacexp)
          (setq% c-macro-preprocessor
                 (concat "ssh " (remote-norm->user@host remote)
                         " \'cc -E -o - -\'")
                 'cmacexp)
          ad-do-it)
      ;; local: msvc, clang, gcc
      (if-platform% 'windows-nt
          ;; cl.exe cannot retrieve from stdin.
          (when (and +cc*-compiler-bin+ +cc*-xargs-bin+)
            (let* ((tmp (make-temp-file
                         (expand-file-name "cc-" temporary-file-directory)))
                   (c-macro-buffer-name "*Macroexpansion*")
                   (c-macro-preprocessor
                    (format "%s -0 > %s && %s && cl -E %s"
                            +cc*-xargs-bin+ tmp +cc*-compiler-bin+ tmp)))
              (unwind-protect ad-do-it
                (delete-file tmp))))
        (when +cc*-compiler-bin+
          (setq% c-macro-buffer-name
                 "*Macroexpansion*"
                 'cmacexp)
          (setq% c-macro-preprocessor
                 (format "%s -E -o - -" +cc*-compiler-bin+)
                 'cmacexp)
          ad-do-it)))))


(defun cc*-dump-predefined-macros (&optional options)
  "Dump predefined macros."
  (interactive "sInput C compiler's options: ")
  (let* ((remote (remote-norm-file (buffer-file-name (current-buffer))))
         (opts (if (> (length options) 0)
                   (concat options " ")
                 options))
         (cmd  (concat "cc " opts "-dM -E -"))
         (dump (if remote
                   (concat "ssh " (remote-norm->user@host remote)
                           " \'" cmd "\'")
                 (if-platform% 'windows-nt
                     (and +cc*-compiler-bin+ (make-cc-dmacro-bin opts))
                   cmd))))
    (with-current-buffer
        (switch-to-buffer
         (concat "*Predefined Macros"
                 (if remote
                     (concat "@" (remote-norm->user@host remote) "*")
                   "*")))
      (view-mode -1)
      (delete-region (point-min) (point-max))
      (message "Invoking [%s] ..." dump)
      (insert (cond ((or remote (and +cc*-compiler-bin+
                                     dump))
                     (let ((x (shell-command* dump)))
                       (if (zerop (car x))
                           (if (> (length (cdr x)) 0)
                               (cdr x)
                             "/* C preprocessor no output! */")
                         (cdr x))))
                    ((not (or remote +cc*-compiler-bin+))
                     "/* C compiler no found! */")
                    (t "/* C preprocessor output failed! */")))
      (message "Invoking [%s] ...done" dump)
      (c-mode)
      (view-mode 1))))


(when-fn% 'make-c-tags 'tags

  (defun cc*-make-system-tags (&optional option file skip renew)
    "Make system C tags.

OPTION for `tags-program'.
FILE where the tags file located, default is `(tags-spec->% :os-include)'.
SKIP regexp to skip directories.
RENEW whether to renew the existing FILE."
    (interactive
     (list (read-string "tags option: "
                        (car *tags-option-history*)
                        '*tags-option-history*)
           (read-string "tags file: " (tags-spec->% :os-include))
           (read-string "tags skip: "
                        (car *tags-skip-history*)
                        '*tags-skip-history*)
           (y-or-n-p "tags renew? ")))
    (let ((includes (cc*-system-include (not renew)))
          (filter (when (and skip (not (string= "" skip)))
                    (lambda (_ a)
                      (not (string-match skip a))))))
      (make-c-tags (car includes) file option nil filter renew)
      (dolist* (p (cdr includes) file)
        (make-c-tags p file option nil filter)))))


;; eldoc

(defun cc*-check-identity (&optional remote)
  "Return a hashtable of cc identities."
  (ignore* remote)
  (let ((tbl (make-hash-table :test 'string-hash=)))
    (puthash "printf"
             "__inline int __cdecl printf(char const* const _Format, ...)"
             tbl)
    (puthash "fflush"
             "int __cdecl fflush(FILE* _Stream);"
             tbl)
    tbl))

(defun cc*-system-identity (&optional cached remote)
  "Return a hashtable of cc identities."
  (let* ((rid (when remote
                (mapconcat #'identity (remote-norm-id remote) "-")))
         (c (if remote
                (v-home* (concat ".exec/.cc-id-" rid ".el"))
              (v-home% ".exec/.cc-id.el")))
         (cc (concat c "c"))
         (var (if remote
                  (intern (concat "cc*-system-identity@" rid))
                'cc*-system-identity)))
    (if (and cached (file-exists-p cc))
        (load cc)
      (let ((tbl (cc*-check-identity remote)))
        (set var tbl)
        (when (save-hash-table-to-file var tbl c 'string-hash=)
          (byte-compile-file c))))
    (symbol-value var)))


(defun cc*-eldoc-doc-fn ()
  "See `eldoc-documentation-function'."
  (let ((tbl (cc*-system-identity t))
        (sym (thing-at-point 'symbol)))
    (when (and tbl (stringp sym))
      (gethash (substring-no-properties sym) tbl))))


(defmacro toggle-cc*-eldoc-mode (&optional arg)
  "Toggle cc-eldoc-mode enabled or disabled."
  `(progn
     (set (make-local-variable 'eldoc-documentation-function)
          (if ,arg #'cc*-eldoc-doc-fn #'ignore))
     (eldoc-mode (if ,arg 1 nil))))


(defun cc*-system-autoload ()
  "Autoload `cc*-system-include', `cc*-system-include' and `eldoc-mode'."
  (cc*-system-include t)
  (when (cc*-system-identity t)
    (toggle-cc*-eldoc-mode 1)))


;; (add-hook 'c-mode-hook (defun-fn-threading^ cc*-system-autoload) t)


 ;; end of eldoc


;;; cc-styles

(defvar cc*-style-nginx
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
	   (brace-list-intro first
                       c-lineup-2nd-brace-entry-in-arglist
                       c-lineup-class-decl-init-+ +)
     (arglist-cont-nonempty . ,(lambda (langem)
                                 (let ((col (save-excursion
                                              (goto-char (cdr langem))
                                              (current-column))))
                                   (cond ((= col 0) 'c-basic-offset)
                                         (t 'c-lineup-arglist)))))))
  "nginx style for `cc-styles'.
https://nginx.org/en/docs/dev/development_guide.html#code_style")


(defun cc*-style-align-entire (begin end &optional n)
  "Align the selected region as if it were one alignment section.

BEGIN and END mark the extent of the region.
N specify the number of spaces when align, default is 2.
See `align-entire'."
  (interactive "r\nP")
  (eval-when-compile (require 'align))
  (require 'align)
  (fluid-let (align-default-spacing (or n 2))
    (align-entire begin end)))


 ;; end of `cc-styles'


(with-eval-after-load 'cc-mode

  (when-var% c-mode-map 'cc-mode

    ;; keymap: find c include file
    (when-fn% 'ff-find-other-file 'find-file
      (define-key% c-mode-map (kbd "C-c f i") #'cc*-find-include-file))

    ;; keymap: indent line or region
    (when-fn% 'c-indent-line-or-region 'cc-cmds
      (define-key% c-mode-map (kbd "TAB") #'c-indent-line-or-region))

    ;; keymap: dump predefined macros
    (define-key% c-mode-map (kbd "C-c #") #'cc*-dump-predefined-macros))

  ;; load `tags'
  (when-fn% 'make-c-tags 'tags
    (require 'tags))

  ;; load styles
  (c-add-style (car cc*-style-nginx) (cdr cc*-style-nginx))

  ;; keymap: align
  (define-key% c-mode-map (kbd "C-c |") #'cc*-style-align-entire))


(with-eval-after-load 'cmacexp

  ;; [C-c C-e] `c-macro-expand' in `cc-mode'
  (setq% c-macro-prompt-flag t 'cmacexp)
  (ad-enable-advice #'c-macro-expand 'around "c-macro-expand-around")
  (ad-activate #'c-macro-expand t))


;; end of on-cc-autoload.el
