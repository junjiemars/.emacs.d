;;;; -*- lexical-binding:t -*-
;;;;
;; gud-lldb.el
;;;;
;; improved from
;; https://opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el.auto.html
;;;;
;;
;; Refine Targets:
;; 1. Start or attach a process.
;; 2. Source code debugging.
;; 3. Commands autocompletion and history.
;; 4. Frame, register buffers.
;; 5. Scripting, see http://lldb.llvm.org/scripting.html
;;
;;;;
;;
;; Sample C code:
;; generated via Nore (https://github.com/junjiemars/nore)
;; ./configure --new
;; make clean test
;; 
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


(defcustom% gud-lldb-directories nil
  "A list of directories that lldb should search for source code.
If nil, only source files in the program directory will be known
to lldb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
                 (repeat :value ("")
                         directory))
  :group 'gud)


(defconst +gud-lldb-prompt-regexp+ "^\\(?:(lldb) *\\)"
  "The regexp pattern of `lldb' prompt.")

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
  "Macro to set lldb's settings."
  (declare (indent 1))
  `(mapconcat #'identity (list "settings" ,subcommand ,@args) " "))


(defun lldb-settings-frame-format ()
  "Set lldb's default frame-format."
  (gud-call (lldb-settings
       "set" "frame-format"
       "frame #${frame.index}: ${frame.pc}{ ${module.file.basename}{`${function.name-with-args}{${frame.no-debug}${function.pc-offset}}}}{ at ${line.file.fullpath}:${line.number}}{${function.is-optimized} [opt]}\\n")))


(defun lldb-settings-stop-display ()
  "Set lldb's default stop display."
  (gud-call (lldb-settings "set" "stop-disassembly-display" "never"))
  (gud-call (lldb-settings "set" "stop-line-count-before" "0"))
  (gud-call (lldb-settings "set" "stop-line-count-after" "0")))


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





;;;;
;; gud-lldb-*
;;;;



;; gud-lldb defined, interact with `gud'
;; `+gud-lldb-prompt-regexp+' -> `comint-prompt-regexp'
;; `gud-lldb-massage-args'
;; `gud-lldb-marker-filter'
;; `gud-lldb-find-file'

(defun gud-lldb-find-file (filename)
  "As the optional argument: find-file of `gud-common-init'.

The job of the find-file method is to visit and return the buffer
indicated by the car of gud-tag-frame.  This may be a file name,
a tag name, or something else."
  (save-excursion
    (let ((f (lldb-file-name filename)))
      (if f
          (find-file-noselect f t)
        (find-file-noselect filename 'nowarn)))))


(defun gud-lldb-massage-args (file args)
  "As the 2nd argument:message-args of `gud-common-init'.

`gud' callback it once when first run `lldb'.

The job of the massage-args method is to modify the given list of
debugger arguments before running the debugger."
  (ignore* file)
  (append (loop* for x in gud-lldb-command-line-hook
                 when (functionp x) append (funcall x))
          args))


(defun gud-lldb-marker-filter (string)
  "As the 3rd argument: marker-filter of `gud-common-init'.

The job of the marker-filter method is to detect file/line
markers in strings and set the global gud-last-frame to indicate
what display action (if any) should be triggered by the marker.
Note that only whatever the method *returns* is displayed in the
buffer; thus, you can filter the debugger's output, interpreting
some and passing on the rest."
  (cond ((string-match "[ \t]*frame.*at \\([^:]+\\):\\([0-9]+\\)" string)
         ;; frame format: `lldb-settings-frame-format'
         ;; (lldb) r
         ;; Process 2353 launched: '/opt/lab/c/spot/out/bin/spot' (x86_64)
         ;; Process 2353 stopped
         ;; * thread #1, queue = 'com.apple.main-thread', stop reason = breakpoint 1.1
         ;;   frame #0: 0x0000000100000f66 spot`main(argc=1, argv=0x00007ffeefbffa58) at c.c:13
         (setq gud-last-frame (cons (match-string 1 string)
                                    (string-to-number (match-string 2 string)))))
        
        ((string-match "Process [0-9]+ exited with status = .*" string)
         ;; Process 13155 exited with status = 0 (0x00000000)              
         (setq gud-last-last-frame nil)
         (setq gud-overlay-arrow-position nil)))
  string)


;; set default `gud-lldb-init-hook'
(add-hook 'gud-lldb-init-hook #'lldb-settings-stop-display t)
(add-hook 'gud-lldb-init-hook #'lldb-settings-frame-format t)



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

  ;; M-{ and M-}
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) +gud-lldb-prompt-regexp+)

  (loop* for x in gud-lldb-init-hook when (functionp x) do (funcall x))
  (setq gud-running nil)
  (setq gud-filter-pending-text nil)
  (run-hooks 'lldb-mode-hook))




(provide 'gud-lldb)

;;; end of file
