;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gud-lldb.el
;;;;
;; features:
;;; 1. start or attach to process.
;;; 2. source code debugging.
;;; 3. command auto completion.
;;; 4. evaluate C expression.
;;; 5* show breakpoints on buffer in GUI/Terminal mode.
;;; 6* show frames and registers on buffer.
;;;;
;; sample C code:
;;; generated via Nore (https://github.com/junjiemars/nore)
;;; ./configure --new
;;; make clean test
;;;;
;; references:
;;; https://lldb.llvm.org
;;; https://lldb.llvm.org/python_api/lldb.SBCommandInterpreter.html
;;; https://opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el.auto.html
;;;;
;; lldb builtin:
;;; apropos
;;;;


;;;;
;; require
;;;;

(require 'gud)
(require% 'guds (v-home%> "config/guds") t)



;;;;
;;  variables
;;;;


(defvar gud-lldb-history nil
  "History of argument lists passed to lldb.")


(defvar gud-lldb-command-line-hook nil
  "Hook run by `lldb' on command line.")


(defvar gud-lldb-init-hook nil
  "Hook run by \\=`lldb\\=' process.")


(defvar gud-lldb-directories '(".")
  "A list of directories that lldb should search for source code.
If nil, only source files in the program directory will be known
  o lldb.")


(defconst +gud-lldb-prompt-regexp+ "^\\(?:(lldb) *\\)"
  "The regexp pattern of \\=`lldb\\=' prompt.")


(defalias '*lldb*
  (let ((b))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t b))))
  "The current *lldb* process buffer.")


(defalias '*lldb-out*
  (let ((b "*out|lldb*"))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t (get-buffer-create b)))))
  "The output buffer of \\=`lldb-completion\\='.")


 ;; end of variable declarations


;;;
;;  gud-* declarations
;;;

(declare-function gud*-def (v-home%> "config/guds"))
(declare-function gud*-find-c-last-expr (v-home%> "config/guds"))
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
;; (autoload 'gud*-def (v-home%> "config/guds") nil nil 'macro)
;; (autoload 'gud*-find-c-last-expr (v-home%> "config/guds"))

 ;; end of gud-* declarations


;;;;
;; lldb-*
;;;;


(defun lldb-file-name (filename)
  "Transform a relative FILENAME to an absolute one.\n
Return absolute filename when FILENAME exists, otherwise nil."
  (or (let ((f (expand-file-name filename)))
        (and (file-exists-p f) f))
      (catch 'br
        (dolist (d gud-lldb-directories)
          (let ((p (concat d "/" filename)))
            (and (file-exists-p p)
                 (throw 'br p)))))))

(defun lldb-start-file (&optional force)
  "Make lldb's init file if FORCE or the init file is missing.\n
Return init file name and ~/.lldbinit-lldb file no touched."
  (let ((init (v-home% ".exec/gud_lldb.rc"))
        (proc (get-buffer-process (*lldb*))))
    (when (or force (null (file-exists-p init)))
      (copy-file (emacs-home% "config/gud_lldb.rc") init t)
      (copy-file (emacs-home% "config/gud_lldb.py")
                 (v-home% ".exec/gud_lldb.py") t))
    (unless proc
      (error "%s" "No lldb process found"))
    (gud-basic-call
     (format (read-str-from-file init) (v-home% ".exec/")))
    init))

(defvar *lldb-completion-filter*
  "^\\(script gud_lldb\\|[[:digit:]]+\\|\\[None\\|[ \t]*$\\)")

(defun lldb-completion-read (in buffer)
  (with-current-buffer buffer
    (let ((alts nil) (xs *lldb-completion-filter*))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((ln (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
          (cond ((string-match xs ln) nil)
                (t (let* ((s (strrchr in #x20))
                          (w1 (if s (1+ s) 0))
                          (ls1 (substring-no-properties in 0 w1))
                          (rs1 (substring-no-properties in w1))
                          (ln> (string-trim> ln)))
                     (cond ((string= rs1 ln>) nil)
                           ((string-match rs1 ln>)
                            (let ((a (list (concat ls1 ln>))))
                              (setq alts (nconc alts a)))))))))
        (forward-line 1))
      alts)))

(defun lldb-completion ()
  (interactive)
  (let* ((lldb (*lldb*)) (proc (get-buffer-process lldb))
         (start nil) (end nil) (in nil))
    (when proc
      (with-current-buffer lldb
        (setq start (save-excursion (comint-goto-process-mark) (point))
              end (point)
              in (buffer-substring-no-properties start end)))
      (let ((cmd (format "script gud_lldb.lldb_emacs_apropos('%s',128)" in))
            (out (*lldb-out*)))
        (with-current-buffer out (erase-buffer))
        (comint-redirect-send-command-to-process cmd out proc nil t)
        (with-current-buffer lldb
          (unwind-protect
              (while (or quit-flag (null comint-redirect-completed))
                (accept-process-output proc 2))
            (comint-redirect-cleanup)))
        (list start end (lldb-completion-read in out)
              :exclusive 'no)))))


;; (defun lldb-toggle-breakpoint ()
;;   "Enable/disable breakpoint at current line of breakpoints buffer."
;;   (save-excursion
;;     (beginning-of-line)
;;     (let ((enabled (if-fn% gud-toggle-breakpoint-notation guds
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
As the 2nd argument of \\=`gud-common-init\\=': massage-args."
  (ignore* file)
  (append (let ((xs nil))
            (dolist (x gud-lldb-command-line-hook xs)
              (and (functionp x) (setq xs (append xs (funcall x))))))
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
  (let* ((expr (if-region-active
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
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
  hese cases the initial working directory is the
\\=`default-directory\\=' of the buffer in which this command was
invoked."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line
                   #'gud-lldb-massage-args
                   #'gud-lldb-marker-filter
                   #'gud-lldb-find-file)

  (*lldb* (get-buffer (format "*gud-%s*" gud-target-name)))
  (lldb-start-file t)

  (set (make-local-variable 'gud-minor-mode) 'lldb)
  (set (make-local-variable 'comint-prompt-regexp) +gud-lldb-prompt-regexp+)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (set (make-local-variable 'comint-process-echoes) t)
  ;; M-{ and M-}
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) +gud-lldb-prompt-regexp+)
  (unless% (memq 'ansi-color-process-output comint-output-filter-functions)
    (push! #'ansi-color-process-output comint-output-filter-functions))

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

  ;; `lldb-completion'
  (add-hook (if-var% completion-at-point-functions minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'lldb-completion 0 'local)

  (setq gud-running nil)
  (setq gud-filter-pending-text nil)
  (run-hooks 'lldb-mode-hook))

 ;; end of gud-lldb-*



(provide 'gud-lldb)


;;; end of gud-lldb.el
