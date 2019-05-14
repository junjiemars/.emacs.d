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

(defcustom% gambit-program
  (cond ((executable-find% "gsc-script"
                           (lambda (gsc)
                             (let ((x (shell-command* gsc
                                        "-e \"(+ 1 2 3)\"")))
                               (zerop (car x)))))
         "gsc-script -:d1- -i")
        (t "gsc -:d1- -i"))
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

(defcustom% gambit-repl-mode-hook nil
  "Hook run upon entry to `gambit-repl-mode'.
This is run before the process is cranked up."
  :type 'hook
  :group 'gambit)

(defvar gambit-buffer nil
  "The current gambit process buffer.")

(defvar *gambit-last-buffer* nil
  "The last gambit buffer.")


 ;; end variable declarations


(defun gambit-input-filter (str)
  "Don't save anything matching `gambit-input-filter-regexp'."
  (not (string-match gambit-input-filter-regexp str)))

(defun gambit-get-old-input ()
  "Snarf the sexp ending at point."
  (buffer-substring (save-excursion (backward-sexp)
                                    (point))
                    (point)))

(defun gambit-switch-to-last-buffer ()
  "Switch to the `*gambit-last-buffer*' from `gambit-buffer'."
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
  (setq mode-line-process '(":%s"))
  (setq comint-input-filter #'gambit-input-filter)
  (setq comint-get-old-input #'gambit-get-old-input))


(defun run-gambit (&optional command-line)
  "Run a gambit process, input and output via buffer *gambit*.

If there is a process already running in *gambit*, switch to that
buffer.  With argument COMMAND-LINE, allows you to edit the
command line (default is value of `gambit-program').

Run the hook `gambit-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (list (if current-prefix-arg
			                   (read-string "Run Gambit: " gambit-program)
			                 gambit-program)))
  (let ((cmdlist (split-string* command-line "\\s-+" t)))
    (unless (comint-check-proc "*gambit*")
      (apply #'make-comint-in-buffer "gambit"
             (get-buffer-create "*gambit*")
             (car cmdlist)
             nil ;; no start file, gsi default init: ~/gambini
             (cdr cmdlist)))
    (setq gambit-program command-line)
    (setq gambit-buffer "*gambit*")
    (with-current-buffer gambit-buffer
      (gambit-repl-mode))
    (switch-to-buffer-other-window "*gambit*")))


 ;; end of REPL


(defun gambit-proc ()
  "Return the `gambit-buffer' process, starting one if necessary."
  (unless (comint-check-proc gambit-buffer)
    (save-window-excursion
      (run-gambit (read-string "Run Gambit: " gambit-program))))
  (or (get-buffer-process gambit-buffer)
      (error "No current process. See variable `gambit-buffer'")))

(defun gambit-switch-to-repl (&optional arg)
  "Switch to the `gambit-buffer' buffer.

If ARG is non-nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (unless (comint-check-proc gambit-buffer)
    (error "No current process. See variable `gambit-buffer'"))
  (unless (with-current-buffer (current-buffer)
            (symbol-value 'gambit-mode))
    (error "No current gambit-mode. See variable `gambit-mode'"))
  (setq *gambit-last-buffer* (current-buffer))
  (if arg
      ;; display REPL but do not select it
      (display-buffer gambit-buffer
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer gambit-buffer)
    (push-mark)
    (goto-char (point-max))))

(defun gambit-compile-file (file)
  "Compile a Scheme FILE in `gambit-buffer'."
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
  "Load a Scheme FILE into `gambit-buffer'."
  (interactive (comint-get-source
                "Load Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source? t)) ;; t because `load'
  (comint-check-source file) 
  (comint-send-string (gambit-proc)
                      (format "(load \"%s\")\n" file))
  (setq *gambit-last-buffer* (current-buffer))
  (gambit-switch-to-repl))

(defun gambit-send-region (start end)
  "Send the current region to `gambit-buffer'."
  (interactive "r")
  (comint-send-region (gambit-proc) start end)
  (comint-send-string (gambit-proc) "\n")
  (gambit-switch-to-repl t))

(defun gambit-send-last-sexp ()
  "Send the previous sexp to `gambit-buffer'."
  (interactive)
  (gambit-send-region (save-excursion (backward-sexp)
                                      (point))
                      (point)))

(defun gambit-send-definition ()
  "Send the current definition to `gambit-buffer'."
  (interactive)
  (gambit-send-region (save-excursion (beginning-of-defun)
                                      (point))
                      (save-excursion (end-of-defun)
                                      (point))))

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
  (gambit-switch-to-repl t))

(defvar gambit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'gambit-send-definition)
    (define-key m "\C-x\C-e" #'gambit-send-last-sexp)
    (define-key m "\C-c\C-l" #'gambit-load-file)
    (define-key m "\C-c\C-k" #'gambit-compile-file)
    (define-key m "\C-c\C-r" #'gambit-send-region)
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
