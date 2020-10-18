;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gambit.el
;;;;


(require 'comint)

;; (require 'scheme)
;; (require 'thingatpt)

;; ;; Disable `geiser-mode' for `scheme-mode'
;; (if-feature-geiser%
;;     (when-var% geiser-mode-auto-p 'geiser-mode
;;       (setq% geiser-mode-auto-p nil 'geiser-mode)))


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
         "gsc-script")
        (t "gsc-script"))
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

(defalias '*gambit*
  (lexical-let% ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "The current *gambit* process buffer.")

(defalias '*gambit-start-file*
  (lexical-let% ((b (let ((f (v-home% ".exec/.gambit.ss")))
                      (unless (file-exists-p f)
                        (copy-file `,(emacs-home* "config/_gambit_.ss")
                                   f t))
                      f)))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n) b)))
  "The `*gambit*' process start file.")

(defalias 'gambit-switch-to-last-buffer
  (lexical-let% ((b))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (when b (switch-to-buffer-other-window b)))))
  "Switch to the last `gambit-mode' buffer from `*gambit*' buffer.")


(defvar *gambit-option-history* nil
  "Gambit option history list.")

(defvar *gambit-trace-history* nil
  "Gambit tracing history list.")

 ;; end variable declarations


(defun gambit-input-filter (str)
  "Don't save anything matching `gambit-input-filter-regexp'."
  (not (string-match gambit-input-filter-regexp str)))

(defun gambit-get-old-input ()
  "Snarf the sexp ending at point."
  (buffer-substring (save-excursion (backward-sexp) (point))
                    (point)))


(defun gambit-check-proc (&optional spawn)
  "Return the `*gambit*' process or start one if necessary."
  (when (and spawn
             (not (eq 'run (car (comint-check-proc (*gambit*))))))
    (save-window-excursion (call-interactively #'run-gambit)))
  (or (get-buffer-process (*gambit*))
      (error "No `*gambit*' process.")))

(defun gambit-input-complete-p ()
  "Return t if the input string contains a complete sexp."
  (save-excursion
    (let ((start (save-excursion (comint-goto-process-mark) (point)))
          (end (point-max)))
      (goto-char start)
      (cond ((looking-at "\\s *['`#]?[(\"]")
             (ignore-errors
               (save-restriction
                 (narrow-to-region start end)
                 (loop* do (skip-chars-forward " \t\r\n)")
                        until (eobp)
                        do (forward-sexp))
                 t)))
            (t t)))))

(defun gambit-repl-return ()
  "Newline or indent then newline the current input."
  (interactive)
  (gambit-check-proc)
  (cond ((gambit-input-complete-p) (comint-send-input))
        (t (newline 1 t))))

(defun gambit-repl-closing-return ()
  "Close all open lists and evaluate the current input."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (save-excursion (comint-goto-process-mark) (point))
                      (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (gambit-repl-return))


(defvar gambit-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [return] #'gambit-repl-return)
    (define-key m [(control return)] #'gambit-repl-closing-return)
    (define-key m "\C-c\C-b" #'gambit-switch-to-last-buffer)
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
  (setq comint-input-filter #'gambit-input-filter)
  (setq comint-get-old-input #'gambit-get-old-input)
  (when% (with-var byte-compile-warnings
           (setq byte-compile-warnings nil)
           (require 'scheme nil t))
    (require 'scheme)
    (scheme-mode-variables))
  (setq mode-line-process '(":%s")))


(defun run-gambit (&optional command-line)
  "Run a gambit process, input and output via buffer *gambit*.

If there is a process already running in `*gambit*', switch to
that buffer. With argument COMMAND-LINE, allows you to edit the
command line.

Run the hook `gambit-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (list (read-string "Run gambit: "
                                  (car *gambit-option-history*)
                                  '*gambit-option-history*)))
  (unless (comint-check-proc (*gambit*))
    (with-current-buffer (get-buffer-create "*gambit*")
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             gambit-program
             (*gambit-start-file*)
             (split-string* command-line "\\s-+" t))
      (*gambit* (current-buffer))
      (gambit-repl-mode)))
  (switch-to-buffer-other-window "*gambit*"))


 ;; end of REPL


(defun gambit-switch-to-repl (&optional arg)
  "Switch to the `*gambit*' buffer.

If ARG is non-nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (gambit-check-proc t)
  (gambit-switch-to-last-buffer (current-buffer))
  (if arg
      ;; display REPL but do not select it
      (display-buffer (*gambit*)
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*gambit*))
    (push-mark)
    (goto-char (point-max))))

(defun gambit-compile-file (file)
  "Compile a Scheme FILE in `*gambit*'."
  (interactive (comint-get-source
                "Compile Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source?
                nil)) 
  (comint-check-source file)
  (comint-send-string (gambit-check-proc)
                      (format "(compile-file \"%s\")\n" file))
  (gambit-switch-to-last-buffer (current-buffer))
  (gambit-switch-to-repl))

(defun gambit-load-file (file)
  "Load a Scheme FILE into `*gambit*'."
  (interactive (comint-get-source
                "Load Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source? t)) ;; t because `load'
  (comint-check-source file)
  (comint-send-string (gambit-check-proc)
                      (format "(load \"%s\")\n" file))
  (gambit-switch-to-last-buffer (current-buffer))
  (gambit-switch-to-repl))

(defun gambit-send-region (start end)
  "Send the current region to `*gambit*'."
  (interactive "r")
  (comint-send-region (gambit-check-proc) start end)
  (comint-send-string (*gambit*) "\n")
  (gambit-switch-to-repl t))

(defun gambit-send-last-sexp ()
  "Send the previous sexp to `*gambit*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (gambit-send-region (car bounds) (cdr bounds)))))

(defun gambit-send-definition ()
  "Send the current definition to `*gambit*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (gambit-send-region (car bounds) (cdr bounds)))))

(defun gambit-trace-procedure (proc &optional untrace)
  "Trace or untrace procedure PROC in `*gambit*' process.

If PROC is nil then untrace or list all traced procedures
determined by the prefix UNTRACE argument."
  (interactive (list (read-string (format "%s procedure: "
                                          (if current-prefix-arg
                                              "Untrace"
                                            "Trace"))
                                  (if current-prefix-arg
                                      (car *gambit-trace-history*)
                                    (when (symbol-at-point)
                                      (symbol-name (symbol-at-point))))
                                  '*gambit-trace-history*)
                     current-prefix-arg))
  (comint-send-string (gambit-check-proc)
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
    ;; (scheme-mode-commands m)
    m))

(make-variable-buffer-local
 (defvar gambit-mode-string nil
   "Modeline indicator for `gambit-mode'."))

(defun gambit-mode--lighter ()
  (or gambit-mode-string " Gambit"))

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




(provide 'gambit)


;; end of gambit.el
