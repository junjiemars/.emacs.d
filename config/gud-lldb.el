;;;; -*- lexical-binding:t -*-
;;;;
;; gud-lldb.el
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


(defvar gud-lldb-history nil
	"History of argument lists passed to lldb.")


(defcustom gud-lldb-init-hook nil
	"Hook run by `lldb'."
	:type 'hook
	:group 'gud)


(defcustom gud-lldb-directories nil
  "A list of directories that lldb should search for source code.
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


(defun lldb-file-name (filename)
  "Transform a relative FILENAME to an absolute one.

Return absolute filename when FILENAME existing or it's existing 
in `gud-lldb-directories'.
"
	(or (let ((f (expand-file-name filename)))
				(when (file-exists-p f) f))
			(loop for d in gud-lldb-directories
						do (let ((p (concat d "/" filename)))
								 (when (file-exists-p p) (return p))))))

(defun lldb-extract-breakpoint-id (string)
	"Extract breakpoint id, see `lldb-breakpoint-id'."
  (when (string-match "Breakpoint \\([0-9.]*\\):" string)
		(setq lldb-breakpoint-id (match-string 1 string))))




;;;;
;; gud-lldb-*
;;;;


(defun gud-lldb-find-file (filename)
	"As the optional argument: find-file of `gud-common-init'.

`gud' callback it just when `gud-lldb-init-list-source' had been called first.

The job of the find-file method is to visit and return the buffer indicated
by the car of gud-tag-frame.  This may be a file name, a tag name, or
something else.
"
  (save-excursion
    (let ((f (lldb-file-name filename)))
			(if f
					(find-file-noselect f t)
				(find-file-noselect filename 'nowarn)))))


(defun gud-lldb-marker-filter (string)
	"As the 3rd argument: marker-filter of `gud-common-init'.

The job of the marker-filter method is to detect file/line markers in
strings and set the global gud-last-frame to indicate what display
action (if any) should be triggered by the marker.  Note that only
whatever the method *returns* is displayed in the buffer; thus, you
can filter the debugger's output, interpreting some and passing on
the rest.
"
	(setq gud-marker-acc (if gud-marker-acc
													 (concat gud-marker-acc string)
												 string))
	;; (lldb-extract-breakpoint-id gud-marker-acc)
	(cond ((string-match "^[ \t]*frame #[0-9]+:.* at \\([^:]+\\):\\([0-9]+\\)"
											 string)
				 ;; (lldb) r
				 ;; Process 2353 launched: '/opt/lab/c/spot/out/bin/spot' (x86_64)
				 ;; Process 2353 stopped
				 ;; * thread #1, queue = 'com.apple.main-thread', stop reason = breakpoint 1.1
				 ;;   frame #0: 0x0000000100000f66 spot`main(argc=1, argv=0x00007ffeefbffa58) at spot.c:13
				 (setq gud-last-frame (cons (match-string 1 string)
																		(string-to-number (match-string 2 string))))))
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
  ;; (setq lldb-oneshot-break-defined nil)

	(gud-def gud-break    "breakpoint set -f %f -l %l" "\C-b"    "Set breakpoint at current line.")
  (gud-def gud-step     "thread step-in"              "\C-s"   "Step one source line with display.")
  (gud-def gud-next     "thread step-over"            "\C-n"   "Step one line (skip functions).")
	(gud-def gud-cont     "process continue"            "\C-r"   "Continue with display.")
  (gud-def gud-finish   "thread step-out"             "\C-f"   "Finish executing current function.")
	(gud-def gud-print    "expression -- %e"            "\C-p"   "Evaluate C expression at point.")

  (setq comint-prompt-regexp  "^(lldb)[ \t]*")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'lldb-mode-hook))




(provide 'gud-lldb)


