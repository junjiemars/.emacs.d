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
;; 3 commands auto completion.
;; 4* show breakpoints in buffer.
;; 5* frame buffer and register buffer.
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
;;;;


;;;;
;; require
;;;;

;; (eval-when-compile (require 'cl))

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
  "Hook run by `lldb' process."
  :type 'hook
  :group 'gud)


(defvar gud-lldb-directories '(".")
  "A list of directories that lldb should search for source code.
If nil, only source files in the program directory will be known
to lldb.")


(defconst +gud-lldb-prompt-regexp+ "^\\(?:(lldb) *\\)"
  "The regexp pattern of `lldb' prompt.")


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
  "The output buffer of `chez-completion'.")


 ;; end of variable declarations


;;;;
;; lldb-*
;;;;


(defun lldb-file-name (filename)
  "Transform a relative FILENAME to an absolute one.

Return absolute filename when FILENAME exists, otherwise nil."
  (or (let ((f (expand-file-name filename)))
        (when (file-exists-p f) f))
      (loop* for d in gud-lldb-directories
             with p = nil
             do (setq p (concat d "/" filename))
             when (file-exists-p p)
             return p)))


(defmacro lldb-settings (subcommand &rest args)
  "Make lldb's setting statments."
  (declare (indent 1))
  `(mapconcat #'identity (list "settings" ,subcommand ,@args) " "))


(defun lldb-settings-frame-format ()
  "Set lldb's default frame-format."
  (gud-call (lldb-settings
                "set" "frame-format"
                "frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name-with-args}{${frame.no-debug}${function.pc-offset}}}}{ at ${line.file.fullpath}:${line.number}}{${function.is-optimized} [opt]}\\n")))


(defun lldb-settings-stop-display ()
  "Set lldb's default stop display."
  (gud-call (lldb-settings "set" "stop-disassembly-display" "no-debuginfo"))
  (gud-call (lldb-settings "set" "stop-line-count-before" "0"))
  (gud-call (lldb-settings "set" "stop-line-count-after" "0")))


(defun lldb-completion ()
  (interactive)
  (let ((proc (get-buffer-process (*lldb*)))
        (start (save-excursion (comint-goto-process-mark) (point)))
        (end (point)))
    (when proc
      (let ((cmd (buffer-substring-no-properties start end)))
        (with-current-buffer (*lldb-out*) (erase-buffer))
        (comint-redirect-send-command-to-process cmd
                                                 (*lldb-out*)
                                                 proc nil t)
        (unwind-protect
            (while (or quit-flag (null comint-redirect-completed))
              (accept-process-output nil 2))
          (comint-redirect-cleanup))
        (list start end
              (catch 'out
                (let ((s1) (s2))
                  (with-current-buffer (*lldb-out*)
                    (goto-char (point-min))
                    (while (< (point) (point-max))
                      (let ((line (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position))))
                        (cond ((string-match "^error:.*$" line) (throw 'out nil))
                              ((string-match "^.*?\s+\\([_a-z]+\\)\s+--.*$" line)
                               (push (match-string 1 line) s2))
                              ((string-match "^Syntax:\s\\([_a-z]+\\)\s+.*$" line)
                               (setq s1 (match-string 1 line)))
                              (t nil))
                        (forward-line 1))))
                  (cond ((null s1) s2)
                        ((string-match (format "^%s\s*.*$" cmd) s1)
                         (list s1))
                        (t (mapcar (lambda (x)
                                     (format "%s\  %s" s1 x))
                                   s2))))))))))


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
  "Find the source file associated with FILENAME.

As the optional argument of `gud-common-init': find-file"
  (save-excursion
    (or (let ((f (lldb-file-name filename)))
          (when f (find-file-noselect f t)))
        (find-file-noselect filename 'nowarn))))


(defun gud-lldb-massage-args (file args)
  "Run `gud-lldb-command-line-hook' before running debugger.

As the 2nd argument of `gud-common-init': message-args"
  (ignore* file)
  (append (loop* for x in gud-lldb-command-line-hook
                 when (functionp x) append (funcall x))
          args))


(defun gud-lldb-marker-filter (string)
  "Detect the file/line markers in STRING.

As the 3rd argument of `gud-common-init': marker-filter"
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


;; set default `gud-lldb-init-hook'
(add-hook 'gud-lldb-init-hook #'lldb-settings-stop-display (emacs-arch))
(add-hook 'gud-lldb-init-hook #'lldb-settings-frame-format (emacs-arch))


(defun lldb (command-line)
  "Run lldb passing it COMMAND-LINE as arguments.

If COMMAND-LINE names a program FILE to debug, lldb will run in a
buffer named *gud-FILE*, and the directory containing FILE
becomes the initial working directory and source-file directory
for your debugger.

If COMMAND-LINE requests that lldb attaches to a process PID,
lldb will run in *gud-PID*, otherwise it will run in *lldb*; in
these cases the initial working directory is the
`default-directory' of the buffer in which this command was
invoked."
  (interactive (list (gud-query-cmdline 'lldb)))
  
  (gud-common-init command-line
                   #'gud-lldb-massage-args
                   #'gud-lldb-marker-filter
                   #'gud-lldb-find-file)

  (*lldb* (get-buffer (format "*gud-%s*" gud-target-name)))
  
  (set (make-local-variable 'gud-minor-mode) 'lldb)

  (gud-def gud-break
           "breakpoint set -f %f -l %l"
           ;; (progn (ignore* arg)
           ;;        (gud-call (lldb-toggle-breakpoint)))
           "\C-b"   "Set breakpoint at current line.")
  (gud-def gud-step
           "thread step-in"
           "\C-s"   "Step one source line with display.")
  (gud-def gud-next
           "thread step-over"
           "\C-n"   "Step one line (skip functions).")
  (gud-def gud-cont
           "process continue"
           "\C-r"   "Continue with display.")
  (gud-def gud-finish
           "thread step-out"
           "\C-f"   "Finish executing current function.")
  (gud-def gud-print
           "expression -- %e"
           "\C-p"   "Evaluate C expression at point.")

  (set (make-local-variable 'comint-prompt-regexp) +gud-lldb-prompt-regexp+)
  (set (make-local-variable 'comint-prompt-read-only) t)
  (unless (memq 'ansi-color-process-output comint-output-filter-functions)
    (add-to-list 'comint-output-filter-functions #'ansi-color-process-output))

  ;; `lldb-completion'
  (add-hook (if-var% completion-at-point-functions 'minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'lldb-completion 0 'local)

  ;; M-{ and M-}
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) +gud-lldb-prompt-regexp+)

  (loop* for x in gud-lldb-init-hook when (functionp x) do (funcall x))
  (setq gud-running nil)
  (setq gud-filter-pending-text nil)
  (run-hooks 'lldb-mode-hook))

 ;; end of gud-lldb-*


(provide 'gud-lldb)

;;; end of file
