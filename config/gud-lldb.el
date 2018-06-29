;;;; -*- lexical-binding:t -*-
;;;;
;; lldb.el
;;;;
;; improved from
;; https://opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el.auto.html
;;;;


;;;;
;; require
;;;;

(eval-when-compile (require 'cl))

(require 'gud)



;;;;
;;	variables																			;
;;;;


(defcustom gud-lldb-history nil
	"History of argument lists passed to lldb."
	:group 'gud)


(defcustom gud-lldb-init-hook nil
	"Hook run by `lldb'."
	:type 'hook
	:group 'gud)


(defcustom gud-lldb-directories nil
  "*A list of directories that lldb should search for source code.
If nil, only source files in the program directory
will be known to lldb.

The file names should be absolute, or relative to the directory
containing the executable being debugged."
  :type '(choice (const :tag "Current Directory" nil)
                 (repeat :value ("")
                         directory))
  :group 'gud)


(defvar lldb-breakpoint-id nil
	"Keeps track of breakpoint.

(lldb) b main
(lldb) Breakpoint 1: where = hi`main + 22 at hi.c:17, address = 0x0000000100000ea6 ")


(defvar lldb-oneshot-break-defined nil
	"Keeps track of whether the Python lldb_oneshot_break function 
definition has been executed.")





;;;;
;; lldb-*
;;;;


(defun lldb-extract-breakpoint-id (string)
	"Extract breakpoint id, see `lldb-breakpoint-id'."
  (when (string-match "Breakpoint \\([0-9.]*\\):" string)
		(setq lldb-breakpoint-id (match-string 1 string))))


;;;;
;; gud-lldb-*
;;;;


(defun gud-lldb-marker-filter (string)
  (setq gud-marker-acc
				(if gud-marker-acc (concat gud-marker-acc string) string))
  (lldb-extract-breakpoint-id gud-marker-acc)
  (let ((start))
		;; Process all complete markers in this chunk
		(while 
				;; (lldb) r
				;; Process 1294 launched: '/opt/apps/c/out/bin/hi' (x86_64)
				;; Process 1294 stopped
				;; * thread #1, queue = 'com.apple.main-thread', stop reason = breakpoint 1.1
				;; frame #0: 0x0000000100000ea6 hi`main(argc=1, argv=0x00007ffeefbffa40) at hi.c:17
				(string-match "^frame #[0-9]* .* at \\([^:\n]*\\):\\([0-9]*\\)\n"
											gud-marker-acc start)
																				;(message "gud-marker-acc matches our pattern....")
			(setq gud-last-frame (cons (match-string 1 gud-marker-acc)
																 (string-to-number (match-string 2 gud-marker-acc)))
						start (match-end 0)))

		;; Search for the last incomplete line in this chunk
		(while (string-match "\n" gud-marker-acc start)
			(setq start (match-end 0)))

		;; If we have an incomplete line, store it in gud-marker-acc.
		(setq gud-marker-acc (substring gud-marker-acc (or start 0))))
	string)

(defun gud-lldb-tbreak ()
	(progn
		(gud-call "breakpoint set -f %f -l %l")
		(sit-for 1)
		(if (not lldb-oneshot-break-defined)
				(progn
					;; The "\\n"'s are required to escape the newline chars
					;; passed to the lldb process.
					(gud-call
					 (concat "script exec \"def lldb_oneshot_break(frame, bp_loc):\\n"
									 "    target=frame.GetThread().GetProcess().GetTarget()\\n"
									 "    bp=bp_loc.GetBreakpoint()\\n"
									 "    print 'Deleting oneshot breakpoint:', bp\\n"
									 "    target.BreakpointDelete(bp.GetID())\""))
					(sit-for 1)
					;; Set the flag since Python knows about the function def now.
					(setq lldb-oneshot-break-defined t)))
		(gud-call "breakpoint command add -p %b -o 'lldb_oneshot_break(frame, bp_loc)'")))


(defun gud-lldb-massage-args (file args)
	"As the 2nd argument:message-args of `gud-common-init'.

`gud' callback it when first run `lldb'.
"
	(ignore* file)
	(append (loop for o in gud-lldb-init-hook
								when (functionp o) append (funcall o)) args))


(defun lldb-file-name (f)
  "Transform a relative file name to an absolute file name, for lldb."
  (let ((result nil))
    (if (file-exists-p f)
        (setq result (expand-file-name f))
      (let ((directories gud-lldb-directories))
        (while directories
          (let ((path (concat (car directories) "/" f)))
            (if (file-exists-p path)
                (setq result (expand-file-name path)
                      directories nil)))
          (setq directories (cdr directories)))))
    result))


(defun gud-lldb-find-file (f)
	(save-excursion
    (let ((realf (lldb-file-name f)))
			(if (file-exists-p (or realf f))
					(if realf
							(find-file-noselect realf t)
						(find-file-noselect f 'nowarn))))))


(defun lldb (command-line)
  "Run lldb on program FILE in buffer *gud-FILE*.

The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive (list (gud-query-cmdline 'lldb)))

  (gud-common-init command-line
									 #'gud-lldb-massage-args
									 #'gud-lldb-marker-filter
									 #'gud-lldb-find-file)
  (set (make-local-variable 'gud-minor-mode) 'lldb)
  (setq lldb-oneshot-break-defined nil)

  (gud-def gud-listb "breakpoint list" "l" "List all breakpoints.")
  (gud-def gud-bt "thread backtrace"
					 "b" "Show stack for the current thread.")
  (gud-def gud-bt-all "thread backtrace all"
					 "B" "Show stacks for all the threads.")
  (gud-def gud-break "breakpoint set -f %f -l %l"
					 "\C-b" "Set breakpoint at current line.")
  (gud-def gud-tbreak (gud-lldb-tbreak)
					 "\C-t" "Set temporary breakpoint at current line.")
  (gud-def gud-remove "breakpoint clear -f %f -l %l"
					 "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step "thread step-in"
					 "\C-s" "Step one source line with display.")
	(gud-def gud-stepi "thread step-inst"
					 "\C-i" "Step one instruction with display.")
	
  (gud-def gud-next "thread step-over"
					 "\C-n" "Step one line (skip functions).")
  (gud-def gud-nexti "thread step-inst-over"
					 nil    "Step one instruction (skip functions).")
  (gud-def gud-cont "process continue"
					 "\C-r" "Continue with display.")
  (gud-def gud-finish "thread step-out"
					 "\C-f" "Finish executing current function.")
  (gud-def gud-up
           (progn (gud-call "frame select -r 1")
                  (sit-for 1))
					 "<" "Up 1 stack frame.")
  (gud-def gud-down
           (progn (gud-call "frame select -r -1")
                  (sit-for 1))
					 ">" "Down 1 stack frame.")
  (gud-def gud-print "expression -- %e"
					 "\C-p" "Evaluate C expression at point.")
  (gud-def gud-pstar "expression -- *%e"
					 nil "Evaluate C dereferenced pointer expression at point.")
  (gud-def gud-run "run"
					 "r" "Run the program.")
  (gud-def gud-stop-subjob "process kill"
					 "s" "Stop the program.")
  (setq comint-prompt-regexp  "^(lldb)[ \t]*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook))




(provide 'gud-lldb)


