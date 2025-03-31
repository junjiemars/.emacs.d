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

;; `newline*'
(require% 'ed (v-home%> "config/ed"))
;; `(tags-spec->% :root)'
(require% 'tags (v-home%> "config/tags"))
(require% 'ssh (v-home%> "config/ssh"))

;; end of require

;;;
;; env
;;;

(defun cc-spec->* (cc cmd)
  "The spec of \\=`cc\\='."
  (cond ((and cc (memq cc '(:cc :clang :gcc)))
         (cond ((and cmd (eq cmd :compile)) "cc %s %s -o%s")
               ((and cmd (eq cmd :include))
                "echo ''| cc -v -E 2>&1 >/dev/null -")
               ((and cmd (eq cmd :define)) "cc %s -dM -E -")
               ((and cmd (eq cmd :macro)) "cc -E -o - -")))
        ((and cc (eq cc :msvc))
         (let ((msvc (v-home% ".exec/cc_msvc.bat"))
               (xargs (v-home% ".exec/cc_xargs")))
           (cond ((and cmd (eq cmd :compile))
                  (concat msvc " && cl %s %s"
                          " -Fo" temporary-file-directory
                          " -Fe%s"))
                 ((and cmd (eq cmd :include)) msvc)
                 ((and cmd (eq cmd :define))
                  (concat msvc " && cl -E nul"))
                 ((and cmd (eq cmd :macro))
                  (let ((tmp (make-temp-file "cc_macro_" nil ".c")))
                    ;; cl.exe can't compile on the fly without xargs
                    (format "%s -0 > %s && %s && cl -E %s"
                            xargs tmp msvc tmp)))
                 ((and cmd (eq cmd :env)) msvc)
                 ((and cmd (eq cmd :xargs)) xargs))))))

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
    "Make cc_msvc.bat for msvc in \\=`exec-path\\='."
    (let ((vcvarsall (cc*-check-vcvarsall-bat))
          (arch (getenv-internal "PROCESSOR_ARCHITECTURE"))
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

(defun cc*--cc-check ()
  (let ((cx '(:cc :clang :gcc :msvc))
        (o (inhibit-file-name-handler
             (make-temp-file
              "cc_check_" nil (if-platform% windows-nt ".exe" ".out"))))
        (i (inhibit-file-name-handler
             (let ((chk (make-temp-file "cc_check_" nil ".c")))
               (copy-file (emacs-home% "config/cc_check.c") chk t)
               chk))))
    (catch 'br
      (dolist (cc cx)
        (let ((cmd (cc-spec->* cc :compile)))
          (let ((rc (shell-command* (format cmd "" i o))))
            (when (zerop (car rc))
              (throw 'br cc))))))))

(defalias 'cc*-cc
  (let ((b (cc*--cc-check)))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "The name of C compiler executable.")

;; end of CC

;;;
;; xargs
;;;

(when-platform% windows-nt
  (defun cc*-make-xargs-bin ()
    "Make a GNU\\='s xargs alternation in Windows."
    (let ((src (emacs-home% "config/cc_xargs.c"))
          (dst (v-home% ".exec/cc_xargs.c"))
          (xargs (cc-spec->* :msvc :xargs)))
      (inhibit-file-name-handler
        (cond ((file-exists-p xargs) xargs)
              (t (unless (null (file-exists-p dst))
                   (copy-file src dst t))
                 (let* ((cmd (format (cc-spec->* :msvc :compile)
                                     "-nologo -DNDEBUG=1 -O2 -utf-8"
                                     dst
                                     xargs))
                        (rc (shell-command* cmd)))
                   (and (zerop (car rc)) xargs))))))))

;; end of xargs

;;;
;; #include
;;;

(defun cc*-include-check (&optional remote)
  "Return a list of system cc include path."
  (let ((rc (if remote
                (shell-command* "ssh"
                  (concat (ssh-remote->user@host remote)
                          " \"" (cc-spec->* (cc*-cc) :include) "\""))
              (shell-command* (cc-spec->* (cc*-cc) :include)))))
    (when (= 0 (car rc))
      (let ((pre (cdr rc)) (inc nil) (beg nil))
        (cond ((eq :msvc (cc*-cc))
               (dolist (x (var->paths
                           (car
                            (nreverse
                             (split-string* pre "\n" t "[ \"]*"))))
                          (nreverse inc))
                 (setq inc (cons (posix-path x) inc))))
              (t (catch 'br
                   (dolist (x (split-string* pre "\n" t "[ \t\n]"))
                     (when (and beg (string-match "End of search list\\." x))
                       (throw 'br (nreverse inc)))
                     (when beg (setq inc (cons x inc)))
                     (when (and (null beg)
                                (string-match
                                 "#include <\\.\\.\\.> search starts here:" x))
                       (setq beg t))))))))))

(defun cc*-system-include-read (file &optional remote)
  (or (read-sexp-from-file file)
      (let ((xs nil))
        (dolist (x (cc*-include-check remote) (setq xs (nreverse xs)))
          (let ((x1 (if remote (concat remote x) x)))
            (and (file-exists-p x1)
                 (setq xs (cons x1 xs)))))
        (prog1 xs
          (and xs (save-sexp-to-file xs file))))))

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
        (cond ((and op (eq op :read))
               (let ((r (cc*-system-include-read fs remote)))
                 (plist-get (setq dx (plist-put dx ss r)) ss)))
              ((and op (eq op :save)) (save-sexp-to-file (plist-get dx ss) fs))
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

(defun cc*-dump-predefined-macros (&optional options)
  "Dump predefined macros."
  (interactive "sInput C compiler's options: ")
  (let* ((remote (ssh-remote-p (buffer-file-name (current-buffer))))
         (cc (if remote :cc (cc*-cc)))
         (cmd (format (cc-spec->* cc :define) (or options "")))
         (rc (cond (remote (fluid-let (shell-file-name "sh")
                             (shell-command* "ssh"
                               (ssh-remote->user@host remote)
                               cmd)))
                   (t (shell-command* cmd)))))
    (with-current-buffer
        (switch-to-buffer
         (if remote
             (format "*Macro Predefined@%s*" (ssh-remote->user@host remote))
           "*Macro Predefined*"))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (if (zerop (car rc))
                    (if (> (length (cdr rc)) 0)
                        (cdr rc)
                      "/* C preprocessor no output! */")
                  (cdr rc)))
        (c-mode)
        (goto-char (point-min))
        (view-mode 1)))))

;; end of #define

;;;
;; `tags'
;;;

(defun cc*-make-tags (&optional renew option)
  "Make system C tags."
  (let ((file (concat (tags-spec->% :root) "os.TAGS"))
        (opt (or option "--langmap=c:.h.c --c-kinds=+ptesgux --extra=+fq")))
    (cond (renew (let ((inc (cc*-system-include :read)))
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
;; `cmacexp'
;;;

(eval-when-compile
  (defmacro when-c-macro-expand% (&rest body)
    (declare (indent 0))
    (if-fn% c-macro-expand cmacexp
            `(progn% ,@body)
      `(comment ,@body))))

(when-c-macro-expand%
 (defun cc*-macro-expand (&rest args)
   "Macro expanding current buffer."
   (interactive "r\nP")
   (let ((remote (ssh-remote-p (buffer-file-name (current-buffer)))))
     (setq% c-macro-prompt-flag t cmacexp)
     (setq% c-macro-buffer-name
            (if remote
                (format "*Macro Expanded@%s*" (ssh-remote->user@host remote))
              "*Macro Expanded*")
            cmacexp)
     (setq% c-macro-preprocessor
            (if remote
                (cc-spec->* :cc :macro)
              (when-platform% windows-nt
                (and (eq :msvc (cc*-cc)) (cc*-make-xargs-bin)))
              (cc-spec->* (cc*-cc) :macro))
            cmacexp)
     (apply #'c-macro-expand args))))

;; end of `cmacexp'

;;;
;; format
;;;

(defun cc*-format-region (&optional beg end)
  "Format the region in (BEG,END) of current buffer via clang-format."
  (interactive (if-region-active
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
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
  ;; align entire style
  (when-fn% align-entire align
    (define-key keymap "|" #'cc*-style-align-entire))
  ;; align backslash style
  (when-fn% c-backslash-region cc-cmds
    (autoload 'c-backslash-region "cc-cmds")
    (define-key keymap "" #'cc*-style-align-backslash))
  (when-fn-ff-find-other-file%
   (define-key keymap "fi" #'cc*-find-include-file))
  (when-c-macro-expand%
   (define-key keymap "" #'cc*-macro-expand))
  (define-key keymap (kbd% "C-c M-c f") #'cc*-format-region))

;; end of keys

;;;
;; `cc-mode'
;;;

(defun on-cc-mode-init! ()
  "On \\=`cc-mode\\=' initialization."
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
    (when (boundp 'c-ts-mode-map)
      (cc*-define-keys c-ts-mode-map))
    (when (boundp 'c++-ts-mode-map)
      (cc*-define-keys c++-ts-mode-map))))

;; end of `c-ts-mode'

(provide 'cc)

;; end of cc.el
