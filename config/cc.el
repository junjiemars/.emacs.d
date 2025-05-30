;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cc.el
;;;;
;; features:
;;; 1. probe C compiler environment.
;;; 2. dump C's `#define'.
;;; 3. preprocess code segments in buffer.
;;; 4. support remote C environment.
;;; 5. format code.
;;;;
;; references:
;;; 1. https://gcc.gnu.org/
;;; 2. https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html
;;; 3. https://clang.llvm.org/
;;; 4. https://clang.llvm.org/docs/ClangFormat.html
;;;;
;; use cases:
;;; 1. open #include file: M-x `cc*-find-include-file'.
;;; 2. expand macro: M-x `cc*-macro-expand'.
;;; 3. dump #define: M-x `cc*-define-dump'.
;;; 4. format code: M-x `cc*-format-region'.
;;;;


;;; require

;; `ff-find-other-file'
;; `cc-search-directories'
(require 'find-file)

(when-fn% xref-push-marker-stack xref
  (autoload 'xref-push-marker-stack "xref"))

;; end of require

;;;
;; env
;;;

(defun cc-spec->* (cc cmd)
  "The spec of \\=`cc\\='."
  (cond ((and cc (eq cc :meta))
         (cond ((and cmd (eq cmd :list)) '(:cc :clang :gcc :msvc))))
        ((and cc (memq cc '(:cc :clang :gcc)))
         (cond ((and cmd (eq cmd :compile)) "cc %s %s -o%s")
               ((and cmd (eq cmd :include))
                "echo ''| cc -v -E 2>&1 >/dev/null -")
               ((and cmd (eq cmd :define)) "cc %s -dM -E -")
               ((and cmd (eq cmd :macro)) "cc %s -E")
               ((and cmd (eq cmd :line-fmt)) "# %d \"%s\"")
               ((and cmd (eq cmd :line-re)) "# \\([0-9]+\\)")
               ((and cmd (eq cmd :ver)) "cc -v")))
        ((and cc (eq cc :msvc))
         (when-platform% windows-nt
           (let ((msvc (v-home% ".exec/cc_msvc.bat")))
             (cond ((and cmd (eq cmd :compile))
                    (concat msvc " && cl %s %s"
                            " -Fo" temporary-file-directory
                            " -Fe%s"))
                   ((and cmd (eq cmd :include)) msvc)
                   ((and cmd (eq cmd :define)) "")
                   ((and cmd (eq cmd :macro)) (concat msvc " &cl %s -E"))
                   ((and cmd (eq cmd :env)) msvc)
                   ((and cmd (eq cmd :line-fmt)) "#line %d \"%s\"")
                   ((and cmd (eq cmd :line-re)) "#line \\([0-9]+\\)")
                   ((and cmd (eq cmd :ver)) (concat msvc " && cl 2>nul"))))))))

(defvar *cc-option-history* nil
  "History list for C compiler\\='s option.")

(defun cc*--option-prompt ()
  (read-string-prompt "Input C compiler's option: " '*cc-option-history*))

;; end of env

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
p        (posix-path
         (or (let* ((rc (shell-command* (shell-quote-argument vswhere)
                          "-nologo -latest -property installationPath"))
                    (bat (and (= 0 (car rc))
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
    "Make cc_msvc.bat for msvc in \\=`exec-path\\='."
    (let ((vcvarsall (cc*-check-vcvarsall-bat))
          (arch (getenv "PROCESSOR_ARCHITECTURE"))
          (src (emacs-home% "config/cc_msvc.bat"))
          (env (cc-spec->* :msvc :env)))
      (when (and vcvarsall arch)
        (let ((bat (read-str-from-file src)))
          (save-str-to-file
           (format bat (file-name-directory vcvarsall) arch)
           env))))))

;; msvc host environment

;;;
;; CC
;;;

(defun cc*--cc-check (&optional restrict)
  (let ((cx (cc-spec->* :meta :list))
        (pre (v-home% ".exec/cc_"))
        (ext (if% (memq system-type '(cygwin ms-dos windows-nt))
                 ".exe"
               ".out"))
        (in (v-home% ".exec/cc_check.c")))
    (unless (file-exists-p in)
      (copy-file (emacs-home% "config/cc_check.c") in))
    (catch :br
      (dolist (cc cx)
        (let* ((out (concat pre (keyword->string cc) ext))
               (cmd (format (cc-spec->* cc :compile) "" in out)))
          (cond ((and (null restrict) (file-exists-p out))
                 (throw :br cc))
                ((null restrict)
                 (and (= 0 (car (shell-command* cmd)))
                      (throw :br cc)))
                (t (and (= 0 (car (shell-command* cmd)))
                        (= 0 (car (shell-command* out)))
                        (throw :br cc)))))))))

(defalias 'cc*-cc
  (let ((b (cc*--cc-check)))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "The name of C compiler executable.")

;; end of CC

;;;
;; #include
;;;

(when-platform% windows-nt
  (defun cc*--include-parse (str)
    (let ((inc nil))
      (dolist (x (var->paths
                  (car (nreverse (split-string* str "\n" t "[ \"]*"))))
                 (nreverse inc))
        (setq inc (cons (posix-path x) inc))))))

(unless-platform% windows-nt
  (defun cc*--include-parse (str)
    (let ((inc nil) (beg nil))
      (catch :br
        (dolist (x (split-string* str "\n" t "[ \t\n]"))
          (when (and beg (string-match "End of search list\\." x))
            (throw :br (nreverse inc)))
          (when beg (setq inc (cons x inc)))
          (when (and (null beg)
                     (string-match
                      "#include <\\.\\.\\.> search starts here:" x))
            (setq beg t)))))))

(defun cc*--include-probe (&optional remote)
  (let* ((cc (if remote :cc (cc*-cc)))
         (inc (cc-spec->* cc :include))
         (rc (if remote
                 (shell-command* "ssh" (ssh-remote->user@host remote)
                                 " \"" inc "\"")
               (shell-command* inc))))
    (when (= 0 (car rc))
      (if-platform% windows-nt
          ;; msvc
          (cc*--include-parse (cdr rc))
        ;; Darwin/Linux
        (cc*--include-parse (cdr rc))))))

(defun cc*--include-id-file (part &optional remote)
  (let ((id (if remote
                (intern (mapconcat #'identity (ssh-remote->ids remote) "-"))
              'native)))
    (cond ((and part (eq :id part)) id)
          ((and part (eq :file part))
           (format "%s-%s.el" (v-home% ".exec/cc-inc") id)))))

(defun cc*--include-read (&optional remote save)
  (let ((file (cc*--include-id-file :file remote))
        (prefix (ssh-remote-p remote)))
    (or (read-sexp-from-file file)
        (let ((xs nil))
          (when (setq xs (dolist (x (cc*--include-probe remote) (nreverse xs))
                           (setq xs (cons (concat prefix x) xs))))
            (and save (save-sexp-to-file xs file))
            xs)))))

(defalias 'cc*-system-include
  (let ((dx nil))
    (lambda (&optional remote)
      (let ((id (cc*--include-id-file :id remote)))
        (or (plist-get dx id)
            (plist-get
             (setq dx (plist-put dx id (cc*--include-read remote t)))
             id)))))
  "Return a list of system include directories.\n
The REMOTE argument from \\=`ssh-remote-p\\='.")

(defun cc*-find-include-file (&optional in-other-window)
  "Find C #include file in \\=`cc*-system-include\\='."
  (interactive "P")
  (setq cc-search-directories
        (let ((file (buffer-file-name (current-buffer))))
          (nconc (when (stringp file)
                   (let ((pwd (file-name-directory file)))
                     (list (string-trim> pwd "/")
                           (string-trim> (path- pwd) "/"))))
                 (cc*-system-include (ssh-remote-p file)))))
  (when-fn% xref-push-marker-stack xref
    (xref-push-marker-stack))
  (ff-find-other-file in-other-window nil))

;; end of #include

;;;
;; #define
;;;

(when-platform% windows-nt
  (defun cc*--define-dump (&optional option)
    (let ((c (v-home% ".exec/cc_define.c"))
          (x (v-home% ".exec/cc_define.exe"))
          (rc nil))
      (unless (file-exists-p c)
        (copy-file (emacs-home% "config/cc_msvc_def.c") c))
      (unless (and option (null (file-exists-p x)))
        (let ((cmd (format (cc-spec->* :msvc :compile) (or option "") c x)))
          (setq rc (shell-command* cmd))))
      (when (and rc (= 0 (car rc)))
        (shell-command* x)))))

(defun cc*-define-dump (&optional option)
  "Dump #define."
  (interactive (cc*--option-prompt))
  (let* ((remote (ssh-remote-p (buffer-file-name (current-buffer))))
         (cc (if remote :cc (cc*-cc)))
         (cmd (format (cc-spec->* cc :define) (or option "")))
         (user@host (and remote (ssh-remote->user@host remote)))
         (rc (if user@host
                 (shell-command* "ssh" user@host cmd)
               (if-platform% windows-nt
                   (cc*--define-dump option)
                 (shell-command* cmd)))))
    (when (and rc (= 0 (car rc)))
      (with-current-buffer
          (switch-to-buffer
           (if remote
               (format "*Macro Defined@%s*" user@host)
             "*Macro Defined*"))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (if (= 0 (car rc))
                      (if (> (length (cdr rc)) 0)
                          (cdr rc)
                        "/* C preprocessor no output! */")
                    (cdr rc)))
          (c-mode)
          (goto-char (point-min))
          (view-mode 1))))))

;; end of #define

;;;
;; macro
;;;

(defun cc*-macro-expand (&optional option)
  "Expand macro."
  (interactive (cc*--option-prompt))
  (let* ((remote (ssh-remote-p (buffer-file-name (current-buffer))))
         (cc (if remote :cc (cc*-cc)))
         (cmd (format (cc-spec->* cc :macro) (or option "")))
         (reg (if-region-active
                  (cons (region-beginning) (region-end))
                (cons (point-min) (point-max))))
         (src (buffer-string))
         (out (concat (make-temp-file "cc_macro_") ".c"))
         (user@host (and remote (ssh-remote->user@host remote)))
         (rout (and user@host (let ((mktemp (shell-command*
                                                "ssh" user@host
                                                "mktemp -t cc_macro_XXXXXX.c")))
                                (unless (= 0 (car mktemp))
                                  (error "Panic, mktemp: %s" mktemp))
                                (string-trim> (cdr mktemp)))))
         (fmt (cc-spec->* cc :line-fmt)) (re (cc-spec->* cc :line-re))
         (rc nil)
         (beg nil) (end nil)
         (buf (get-buffer-create* (if user@host
                                      (format "*Macro Expanded@%s*" user@host)
                                    "*Macro Expanded*")))
         (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (goto-char (point-min))
      (insert src)
      (goto-char (car reg))
      (beginning-of-line)
      (open-line 1)
      (insert (setq beg (format fmt (line-number-at-pos) out)))
      (goto-char (+ (cdr reg) (length beg)))
      (forward-line 1)
      (open-line 1)
      (beginning-of-line)
      (insert (setq end (format fmt (1- (line-number-at-pos)) out)))
      (save-str-to-file (buffer-string) out)
      (setq rc (cond (user@host (let ((rc1 (shell-command* "scp"
                                             out (concat user@host ":" rout))))
                                  (unless (= 0 (car rc1))
                                    (error "Panic, scp: %s" rc1))
                                  (shell-command* "ssh" user@host cmd rout)))
                     (t (shell-command* cmd out))))
      (erase-buffer)
      (goto-char (point-min))
      (cond ((= 0 (car rc))
             (insert (cdr rc))
             (goto-char (point-max))
             (when (re-search-backward end nil t 1)
               (beginning-of-line)
               (insert "\n/* # " (string-match* re end 1) " */\n")
               (delete-region (point) (point-max))
               (when (re-search-backward beg nil t 1)
                 (end-of-line)
                 (delete-region (point-min) (point))
                 (beginning-of-line)
                 (insert "/* # " (string-match* re beg 1) " */\n"))))
            (t (let ((errno (number-to-string (car rc))))
                 (insert "/* Error: " errno " */\n\n"
                         (cdr rc)
                         "\n\n/* Error " errno " */"))))
      (goto-char (point-min))
      (c-mode)
      (view-mode 1))
    (switch-to-buffer buf)))

;; end of macro

;;;
;; `tags'
;;;

(defun cc*-make-tags (&optional renew option)
  "Make system C tags."
  (let ((file (concat (tags-spec->* :root) "os.TAGS"))
        (opt (or option "--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq")))
    (cond (renew (let ((inc (cc*-system-include)))
                   (make-c-tags (car inc) file opt nil nil t)
                   (dolist (p (cdr inc) file)
                     (make-c-tags p file opt))))
          (t file))))

;; end of `cc*-make-tags'

;;;
;; `cc-styles'
;;;

(when-fn% align-entire align
  (defun cc*-style-align-entire (&rest _)
    "See \\=`align-entire\\='."
    (interactive)
    (let ((align-default-spacing 2))
      (ignore* align-default-spacing)
      (call-interactively #'align-entire))))

(when-fn% c-backslash-region cc-cmds
  (defun cc*-style-align-backslash (&rest _)
    "See \\=`c-backslash-region\\='."
    (interactive)
    (let ((c-backslash-column 48)
          (c-backslash-max-column 72))
      (ignore* c-backslash-column c-backslash-max-column)
      (call-interactively #'c-backslash-region))))

;; end of `cc-styles'

;;;
;; format
;;;

(defun cc*-format-region (&optional beg end)
  "Format the region in (BEG,END) of current buffer via clang-format."
  (interactive (select-region-prompt))
  (let* ((buf (current-buffer))
         (tmp (get-buffer-create* (symbol-name (gensym* "cc-fmt-")) t))
         (cur (with-current-buffer buf (point))))
    (unwind-protect
        (let ((rc (call-process-region
                   nil nil "clang-format" nil tmp nil
                   "-output-replacements-xml"
                   "-fallback-style" "gnu"
                   "-offset" (number-to-string (1- (position-bytes beg)))
                   "-length" (number-to-string
                              (- (position-bytes end) (position-bytes beg)))
                   "-cursor" (number-to-string (1- (position-bytes cur)))))
              (c1 cur)
              (xml `(("&lt;"   . "<")
                     ("&gt;"   . ">")
                     ("&apos;" . "'")
                     ("&quot;" . "\"")
                     ("&amp;"  . "&")
                     ("&#\\([0-9]+\\);" . 10)
                     ("&#[xX]\\([0-9a-fA-F]+\\);" . 16))))
          (when (and rc (= rc 0))
            (with-current-buffer tmp
              (goto-char (point-min))
              (when (re-search-forward "incomplete_format='false'" nil t 1)
                (when (re-search-forward
                       "<cursor>\\([0-9]+\\)</cursor>" nil t 1)
                  (setq c1 (string-to-number
                            (buffer-substring-no-properties
                             (match-beginning 1) (match-end 1)))))
                (goto-char (point-max))
                (while (re-search-backward
                        (concat
                         "<replacement offset='\\([0-9]+\\)'"
                         " length='\\([0-9]+\\)'>\\(.*?\\)"
                         "</replacement>")
                        nil t)
                  (let ((off (string-to-number
                              (buffer-substring-no-properties
                               (match-beginning 1) (match-end 1))))
                        (len (string-to-number
                              (buffer-substring-no-properties
                               (match-beginning 2) (match-end 2))))
                        (txt (buffer-substring-no-properties
                              (match-beginning 3) (match-end 3))))
                    (with-current-buffer buf
                      (let ((lhs (byte-to-position (1+ off)))
                            (rhs (byte-to-position (1+ (+ off len)))))
                        (when (and (<= beg lhs) (>= end rhs))
                          (and (< lhs rhs) (delete-region lhs rhs))
                          (when txt
                            (goto-char lhs)
                            (insert (strawk txt xml))))))))
                (and c1 (setq c1 (byte-to-position c1)))))
            (goto-char (setq cur (or c1 cur)))))
      (and tmp (kill-buffer tmp))
      cur)))

;; end of format

;;;
;; `man'
;;;

(defun on-man-init! ()
  "On \\=`man\\=' initialization."
  (when-platform% darwin
    (setq% manual-program "/usr/bin/man" man))
  ;; fix cannot find include path in `Man-mode'
  (setq% Man-header-file-path (cc*-system-include) man))

;; end of `man'

;;;
;; keys
;;;

(defun cc*-define-keys (keymap)
  ;; dump predefined macros
  (define-key keymap "#" #'cc*-define-dump)
  ;; raw newline
  (define-key keymap (kbd% "RET") #'newline*)
  ;; align entire style
  (when-fn% align-entire align
    (define-key keymap "|" #'cc*-style-align-entire))
  ;; align backslash style
  (when-fn% c-backslash-region cc-cmds
    (autoload 'c-backslash-region "cc-cmds")
    (define-key keymap "" #'cc*-style-align-backslash))
  ;; `subword-mode'
  (if-fn% subword-mode subword
          (define-key keymap "" #'subword-mode)
    (define-key keymap "" #'c-subword-mode))
  (define-key keymap "fi" #'cc*-find-include-file)
  (define-key keymap "" #'cc*-macro-expand)
  (define-key keymap (kbd% "C-c M-c f") #'cc*-format-region))

;; end of keys

;;;
;; `cc-mode'
;;;

(defun on-cc-mode-init! ()
  "On \\=`cc-mode\\=' initialization."
  (when-var% c-mode-map cc-mode
    ;; indent line or region
    (when-fn% c-indent-line-or-region cc-cmds
      (define-key% c-mode-map (kbd "TAB") #'c-indent-line-or-region)))
  (and (boundp 'c-mode-map) (cc*-define-keys c-mode-map))
  (and (boundp 'c++-mode-map) (cc*-define-keys c++-mode-map))
  t)

;; end of `cc-mode'

;;;
;; `c-ts-mode'
;;;

(when-feature-treesit%
  (defun on-c-ts-mode-init! ()
    "On \\=`c-ts-mode\\=' initialization."
    (and (boundp 'c-ts-mode-map) (cc*-define-keys c-ts-mode-map))
    (and (boundp 'c++-ts-mode-map) (cc*-define-keys c++-ts-mode-map))
    t))

;; end of `c-ts-mode'

(provide 'cc)

;; end of cc.el
