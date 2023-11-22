;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gud-lldb.el
;;;;
;;
;; features:
;; 1. start or attach to process.
;; 2. source code debugging.
;; 3. command auto completion.
;; 4. evaluate C expression.
;; 5* show breakpoints on buffer in GUI/Terminal mode.
;; 6* frame buffer and register buffer.
;;
;;;;
;;
;; sample C code:
;; generated via Nore (https://github.com/junjiemars/nore)
;; ./configure --new
;; make clean test
;;
;; website:
;; https://lldb.llvm.org
;;
;; original from:
;; https://opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el.auto.html
;;
;; lldb builtin:
;; apropos
;;;;


;;;;
;; require
;;;;


(eval-when-compile
  (require 'guds (v-home%> "config/guds.el")))
(require 'gud)



;;;;
;;  variables
;;;;


(defvar gud-lldb-history nil
  "History of argument lists passed to lldb.")


(defcustom% gud-lldb-command-line-hook nil
  "Hook run by `lldb' on command line."
  :type 'hook
  :group 'gud)


(defcustom% gud-lldb-init-hook nil
  "Hook run by \\=`lldb\\=' process."
  :type 'hook
  :group 'gud)


(defvar gud-lldb-directories '(".")
  "A list of directories that lldb should search for source code.
If nil, only source files in the program directory will be known
to lldb.")


(defconst +gud-lldb-prompt-regexp+ "^\\(?:(lldb) *\\)"
  "The regexp pattern of \\=`lldb\\=' prompt.")


(defalias '*lldb*
  (lexical-let% ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "The current *lldb* process buffer.")


(defalias '*lldb-out*
  (lexical-let% ((b "*out|lldb*"))
    (lambda (&optional n)
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of \\=`lldb-completion\\='.")


 ;; end of variable declarations


;;;;
;;  gud-* declarations
;;;;

(declare-function gud*-find-c-last-expr "guds")
(declare-function gud-basic-call    "gud")
(declare-function gud-break         "gud")
(declare-function gud-call          "gud")
(declare-function gud-cont          "gud")
(declare-function gud-common-init   "gud")
(declare-function gud-down          "gud")
(declare-function gud-finish        "gud")
(declare-function gud-jump          "gud")
(declare-function gud-next          "gud")
(declare-function gud-print         "gud")
(declare-function gud-pv            "gud")
(declare-function gud-query-cmdline "gud")
(declare-function gud-remove        "gud")
(declare-function gud-statement     "gud")
(declare-function gud-step          "gud")
(declare-function gud-stepi         "gud")
(declare-function gud-tbreak        "gud")
(declare-function gud-until         "gud")
(declare-function gud-up            "gud")

 ;; end of gud-* declarations


;;;;
;; lldb-*
;;;;


(defun lldb-file-name (filename)
  "Transform a relative FILENAME to an absolute one.\n
Return absolute filename when FILENAME exists, otherwise nil."
  (or (let ((f (expand-file-name filename)))
        (when (file-exists-p f) f))
      (loop* for d in gud-lldb-directories
             with p = nil
             do (setq p (concat d "/" filename))
             when (file-exists-p p)
             return p)))


(defmacro lldb-settings (subcommand &rest args)
  "Make lldb's setting statement."
  (declare (indent 1))
  `(mapconcat #'identity (list "settings" ,subcommand ,@args) " "))


(defmacro lldb-init-statement ()
  "Make lldb's init statements."
  `(concat
    (lldb-settings
     "set" "frame-format"
     "frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name-with-args}{${frame.no-debug}${function.pc-offset}}}}{ at ${line.file.fullpath}:${line.number}}{${function.is-optimized} [opt]}\\n\n")
    (lldb-settings "set" "stop-disassembly-display" "no-debuginfo\n")
    (lldb-settings "set" "stop-line-count-before" "0\n")
    (lldb-settings "set" "stop-line-count-after" "0\n")
    (concat "script "
            "_d_=lldb.debugger.GetCommandInterpreter();"
            "_m_=lldb.SBStringList();"
            "import random;"
            "_r_=lambda x:[a for a in range(0,x)] if x<16"
            " else [random.randrange(0,x) for c in range(0,16)];")))


(defun lldb-settings-init-file (&optional force)
  "Make lldb's init file if FORCE or the init file is missing."
  (let ((file (expand-file-name "~/.lldbinit-lldb")))
    (when (or force (null (file-exists-p file)))
      (save-str-to-file (lldb-init-statement) file))))


(defun lldb-script-apropos (ss)
  "Apropos via lldb's script.\n
The \\=`max_return_elements\\=' argument in
\\=`HandleCompletion\\=' had not been implemented, see
https://github.com/llvm/llvm-project/."
  (concat "script "
          "_m_.Clear();"
          (format "_d_.HandleCompletion('%s',%d,%d,%d,_m_);"
                  ss
                  (if (string= "" ss) 0 (length ss))
                  (if (string= "" ss) 0 (length ss))
                  -1)
          "print('(');"
          "["
          "print('\"%s\"' % (_m_.GetStringAtIndex(x)))"
          " for x in _r_(_m_.GetSize())"
          "];"
          "print(')');"
          "_m_.Clear();"))


(defun lldb-completion ()
  (interactive)
  (let ((proc (get-buffer-process (*lldb*)))
        (start (save-excursion (comint-goto-process-mark) (point)))
        (end (point)))
    (when proc
      (let* ((cmd (buffer-substring-no-properties start end))
             (script (lldb-script-apropos cmd)))
        (with-current-buffer (*lldb-out*) (erase-buffer))
        (comint-redirect-send-command-to-process script
                                                 (*lldb-out*)
                                                 proc nil t)
        (unwind-protect
            (while (or quit-flag (null comint-redirect-completed))
              (accept-process-output nil 2))
          (comint-redirect-cleanup))
        (list start end
              (let* ((xs (concat "^\\(script.*"
                                 "\\|[[:digit:]]+"
                                 "\\|\"\s*\""
                                 "\\|\"None\""
                                 "\\|\\[.*\\]"
                                 "\\)"))
                     (s1 (read-from-string
                          (with-current-buffer (*lldb-out*)
				                    (flush-lines xs (point-min) (point-max) nil)
				                    (buffer-substring-no-properties
                             (point-min) (point-max))))))
                (when (and (consp s1) (car s1))
                  (let* ((ss (car s1))
                         (w (let ((n (- end start)))
                              (while (and (> n 0)
                                          (not (char= (aref cmd (1- n)) ?\ )))
                                (setq n (1- n)))
                              (if (> n 0) n 0)))
                         (ls (substring cmd 0 w))
                         (rs (substring cmd w)))
                    (mapcar (lambda (z) (concat ls z))
                            (if (and (cadr ss)
                                     (string-match
                                      (concat rs (string-trim> (car ss)))
                                      (cadr ss)))
                                (cdr ss)
                              ss)))))
              :exclusive 'no)))))


;; (defun lldb-toggle-breakpoint ()
;;   "Enable/disable breakpoint at current line of breakpoints buffer."
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((enabled (if-fn% 'gud-toggle-breakpoint-notation 'guds
;;                            (progn (require 'guds)
;;                                   (gud-toggle-breakpoint-notation))
;;                      t)))
;;       (if enabled
;;           "breakpoint set -f %f -l %l"
;;         "breakpoint clear -f %f -l %l"))))


 ;; end of lldb-*


;;;;
;; gud-lldb-*
;;;;



;; gud-lldb defined, interact with `gud'
;; `+gud-lldb-prompt-regexp+' -> `comint-prompt-regexp'
;; `gud-lldb-massage-args'
;; `gud-lldb-marker-filter'
;; `gud-lldb-find-file'

(defun gud-lldb-find-file (filename)
  "Find the source file associated with FILENAME.\n
As the optional argument of \\=`gud-common-init\\=': find-file."
  (save-excursion
    (or (let ((f (lldb-file-name filename)))
          (when f (find-file-noselect f t)))
        (find-file-noselect filename 'nowarn))))


(defun gud-lldb-massage-args (file args)
  "Run \\=`gud-lldb-command-line-hook\\=' before running debugger.\n
As the 2nd argument of \\=`gud-common-init\\=': message-args"
  (ignore* file)
  (append (loop* for x in gud-lldb-command-line-hook
                 when (functionp x) append (funcall x))
          args))


(defun gud-lldb-marker-filter (string)
  "Detect the file/line markers in STRING.\n
As the 3rd argument of \\=`gud-common-init\\=': marker-filter"
  (cond ((string-match "[ \t]*frame.*at \\([^:]+\\):\\([0-9]+\\)" string)
         ;; frame format: `lldb-settings-frame-format'
         ;; (lldb) r
         ;; Process 2353 launched: '/opt/lab/c/spot/out/bin/spot' (x86_64)
         ;; Process 2353 stopped
         ;; * thread #1, queue = 'com.apple.main-thread', stop reason = breakpoint 1.1
         ;;   frame #0: 0x0000000100000f66 spot`main(argc=1, argv=0x00007ffeefbffa58) at c.c:13
         (setq gud-last-frame
               (cons (match-string 1 string)
                     (string-to-number (match-string 2 string)))))

        ((string-match "Process [0-9]+ exited with status = .*" string)
         ;; Process 13155 exited with status = 0 (0x00000000)
         (setq gud-last-last-frame nil)
         (setq gud-overlay-arrow-position nil)))
  string)


(defun gud-lldb-print (&optional options)
  "Evaluate C expression at point."
  (interactive (list (if current-prefix-arg
                         (read-string "expression options: ")
                       "--")))
  (let* ((expr (region-active-if
                   (buffer-substring-no-properties (region-beginning)
                                                   (region-end))
                 (gud*-find-c-last-expr)))
         (cmd (concat "expression " options " ( " expr " )")))
    (message "Command: %s" cmd)
    (gud-basic-call cmd)))


(defun gud-lldb (command-line)
  "Run lldb passing it COMMAND-LINE as arguments.\n
If COMMAND-LINE names a program FILE to debug, lldb will run in a
buffer named *gud-FILE*, and the directory containing FILE
becomes the initial working directory and source-file directory
for your debugger.
If COMMAND-LINE requests that lldb attaches to a process PID,
lldb will run in *gud-PID*, otherwise it will run in *lldb*; in
these cases the initial working directory is the
\\=`default-directory\\=' of the buffer in which this command was
invoked."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line
                   #'gud-lldb-massage-args
                   #'gud-lldb-marker-filter
                   #'gud-lldb-find-file)

  (lldb-settings-init-file)

  (*lldb* (get-buffer (format "*gud-%s*" gud-target-name)))

  (set (make-local-variable 'gud-minor-mode) 'lldb)

  (gud-def gud-break
           "breakpoint set -f %f -l %l"
           ;; (progn (ignore* arg)
           ;;        (gud-call (lldb-toggle-breakpoint)))
           "\C-b"   "Set breakpoint at current line.")
  (gud-def gud-tbreak
           "tbreak %f:%l"
           "\C-t"
	         "Set temporary breakpoint at current line.")
  (gud-def gud-step
           "thread step-in"
           "\C-s"   "Step one source line with display.")
  (gud-def gud-next
           "thread step-over"
           "\C-n"   "Step one line (skip functions).")
  (gud-def gud-cont
           "process continue"
           "\C-c"   "Continue with display.")
  (gud-def gud-finish
           "thread step-out"
           "\C-f"   "Finish executing current function.")
  (gud-def gud-up
           "up %p"
           "<" "Up N stack frames (numeric arg).")
  (gud-def gud-down
           "down %p"
           ">" "Down N stack frames (numeric arg).")

  (gud*-def "\C-p" #'gud-lldb-print)

  (set (make-local-variable 'comint-prompt-regexp) +gud-lldb-prompt-regexp+)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (unless (memq 'ansi-color-process-output comint-output-filter-functions)
    (push! #'ansi-color-process-output comint-output-filter-functions))

  ;; `lldb-completion'
  (add-hook (if-var% completion-at-point-functions 'minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'lldb-completion 0 'local)

  ;; M-{ and M-}
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) +gud-lldb-prompt-regexp+)

  (setq gud-running nil)
  (setq gud-filter-pending-text nil)
  (run-hooks 'lldb-mode-hook))

 ;; end of gud-lldb-*



(provide 'gud-lldb)


;;; end of gud-lldb.el
