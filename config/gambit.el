;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gambit-mode.el --- Run Gambit in an Emacs buffer
;;;;

(require 'scheme)
(require 'comint)
(require 'thingatpt)


;; variable declarations

(defgroup gambit nil
  "Run a gambit process in a buffer."
  :group 'scheme)

(defcustom% gambit-program "gsc-script -:d1- -i"
  "Program invoked by the `run-gambit' command."
  :type 'string
  :group 'gambit)

(defcustom% scheme-source? '(scheme-mode)
  "Used to determine if a buffer contains Scheme source code.
If it's loaded into a buffer that is in one of these major modes,
it's considered a scheme source file by `scheme-load-file' and
`scheme-compile-file'.  Used by these commands to determine
defaults."
  :type '(repeat function)
  :group 'gambit)

(defcustom% gambit-input-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'gambit)

(defvar *gambit-repl* nil
  "The current gambit process buffer.")

(defvar *gambit-last-buffer* nil
  "The last gambit buffer.")

 ;; end variable declarations


(defun gambit-input-filter (str)
  "Don't save anything matching `gambit-input-filter-regexp'."
  (not (string-match gambit-input-filter-regexp str)))

(defun gambit-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun gambit-switch-to-last-buffer ()
  "Switch to the `*gambit-last-buffer*' from `*gambit-repl*'."
  (interactive)
  (when *gambit-last-buffer*
    (switch-to-buffer-other-window *gambit-last-buffer*)))

(defvar gambit-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-a" #'gambit-switch-to-last-buffer)
    m))

(define-derived-mode gambit-repl-mode comint-mode "REPL"
  "Major mode for interacting with a gambit process.

The following commands are available:
\\{gambit-repl-mode-map}

A gambit process can be fired up with M-x `run-gambit'.

Customization: 
Entry to this mode runs the hooks on `comint-mode-hook' and
  `gambit-repl-mode-hook' (in that order).
You can send text to the gambit process from other buffers
  containing Scheme source.

Commands:
Return after the end of the process' output sends the
  text from the end of process to point.
Return before the end of the process' output copies the sexp
  ending at point to the end of the process' output, and sends
  it.
Delete converts tabs to spaces as it moves back.
Tab indents for Scheme; with argument, shifts rest of expression
  rigidly with the current line.
C-M-q does Tab on each line starting within following expression:
  Paragraphs are separated only by blank lines.
  Semicolons start comments.
If you accidentally suspend your process, use
  \\[comint-continue-subjob] to continue it."
  (setq comint-prompt-regexp "^[^>\n]*>+ *")
  (setq comint-prompt-read-only t)
  (scheme-mode-variables)
  (setq comint-input-filter #'gambit-input-filter)
  (setq comint-get-old-input #'gambit-get-old-input))


(defun run-gambit (cmd)
  "Run an inferior Scheme process, input and output via buffer *gambit*.

If there is a process already running in *gambit*, switch to
that buffer.  With argument, allows you to edit the command
line (default is value of `scheme-program-name').

Run the hook `gambit-mode-hook' after the `comint-mode-hook'."
  (interactive (list (if current-prefix-arg
			                   (read-string "Run Gambit: " gambit-program)
			                 gambit-program)))
  (when (not (comint-check-proc "*gambit*"))
    (let ((cmdlist (split-string* cmd "\\s-+" t)))
      (set-buffer (apply 'make-comint "gambit"
                         (car cmdlist)
                         nil ;; no start file, gsi default init: ~/gambini
                         (cdr cmdlist)))
	    (gambit-repl-mode)))
  (setq gambit-program cmd)
  (setq *gambit-repl* "*gambit*")
  (setq mode-line-process '(":%s"))
  (switch-to-buffer-other-window "*gambit*"))

 ;; end of REPL


(defun gambit-start-repl-process ()
  "Start an Gambit process.

Return the process started. Since this command is run implicitly,
always ask the user for the command to run."
  (save-window-excursion
    (run-gambit (read-string "Run Gambit: " gambit-program))))

(defun gambit-proc ()
  "Return the `*gambit-repl*' process, starting one if necessary."
  (unless (and *gambit-repl*
               (get-buffer *gambit-repl*)
               (comint-check-proc *gambit-repl*))
    (gambit-start-repl-process))
  (or (get-buffer-process *gambit-repl*)
      (error "No current process.  See variable `*gambit-repl*'")))

(defun gambit-switch-to-repl (&optional arg)
  "Switch to the `*gambit-repl*' buffer.

If ARG is non-nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (unless (or (and *gambit-repl* (get-buffer *gambit-repl*))
              (gambit-start-repl-process))
    (error "No current process buffer. See variable `*gambit-repl*'"))
  (if arg
      (display-buffer *gambit-repl*
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    (pop-to-buffer *gambit-repl*)
    (push-mark)
    (goto-char (point-max))))

(defun gambit-compile-file (file)
  "Compile a Scheme FILE in `*gambit-repl*'."
  (interactive (comint-get-source
                "Compile Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source?
                nil)) 
  (comint-check-source file)
  (comint-send-string (gambit-proc)
                      (format "(compile-file \"%s\")\n" file))
  (setq *gambit-last-buffer* (current-buffer))
  (gambit-switch-to-repl))

(defun gambit-load-file (file)
  "Load a Scheme FILE into `*gambit-repl*'."
  (interactive (comint-get-source
                "Load Scheme file: "
                ;;scheme-prev-l/c-dir/file
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source? t)) ;; t because `load'
  ;; needs an exact name
  ;; Check to see if buffer needs saved
  (comint-check-source file) 
  (comint-send-string (gambit-proc)
                      (format "(load \"%s\")\n" file))
  (setq *gambit-last-buffer* (current-buffer))
  (gambit-switch-to-repl))

(defun gambit-send-region (start end)
  "Send the current region to `*gambit-repl*'."
  (interactive "r")
  (comint-send-region (gambit-proc) start end)
  (comint-send-string (gambit-proc) "\n")
  (gambit-switch-to-repl t))

(defun gambit-send-last-sexp ()
  "Send the previous sexp to `*gambit-repl*'."
  (interactive)
  (gambit-send-region (save-excursion (backward-sexp) (point)) (point)))

(defun gambit-send-definition ()
  "Send the current definition to `*gambit-repl*'."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (gambit-send-region (point) end))))

(defun gambit-trace-procedure (proc &optional untrace)
  "Trace procedure PROC in the gambit process.
With a prefix argument switch off tracing of procedure PROC."
  (interactive
   (list (let ((current (symbol-at-point))
               (action (if current-prefix-arg "Untrace" "Trace")))
           (if current
               (read-string (format "%s procedure [%s]: " action current)
                            nil
                            nil
                            (symbol-name current))
             (read-string (format "%s procedure: " action))))
         current-prefix-arg))
  (when (= (length proc) 0)
    (error "Invalid procedure name"))
  (comint-send-string (gambit-proc)
                      (format "(%s %s)\n"
                              (if untrace "untrace" "trace")
                              proc))
  (comint-send-string (gambit-proc) "\n")
  (gambit-switch-to-repl t))

(defvar gambit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'gambit-send-definition)
    (define-key m "\C-x\C-e" #'gambit-send-last-sexp)
    (define-key m "\C-c\C-l" #'gambit-load-file)
    (define-key m "\C-c\C-k" #'gambit-compile-file)
    (define-key m "\C-c\C-t" #'gambit-trace-procedure)
    (define-key m "\C-c\C-z" #'gambit-switch-to-repl)
    (scheme-mode-commands m)
    m))

(make-variable-buffer-local
 (defvar gambit-mode-string nil
   "Modeline indicator for `gambit-mode'."))

(defun gambit-mode--lighter ()
  (or gambit-mode-string
      (format " %s" (or "Gambit" "G"))))

(define-minor-mode gambit-mode
  "Toggle Gambit's mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Gambit mode is enabled, a host of nice utilities for
interacting with the Gambit REPL is at your disposal.
\\{gambit-mode-map}"
  :init-value nil
  :lighter (:eval (gambit-mode--lighter))
  :group 'gambit-mode
  :keymap gambit-mode-map)


(provide 'gambit-mode)


;; end of file
