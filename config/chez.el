;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; chez.el
;;;;

;; (require 'scheme)
(require 'comint)

;; (require 'thingatpt)
;; `symbol-at-point' autoload fn


;; ;; Disable `geiser-mode' for `scheme-mode'
;; (if-feature-geiser%
;;     (when-var% geiser-mode-auto-p 'geiser-mode
;;       (setq% geiser-mode-auto-p nil 'geiser-mode)))


;; variable declarations

(defgroup chez nil
  "Run a chez process in a buffer."
  :group 'scheme)

(defcustom% chez-program
  (cond ((executable-find% "scheme"
                           (lambda (chez)
                             (let ((x (shell-command* "echo"
                                        "'(+ 1 2 3)'|" chez "-q")))
                               (zerop (car x)))))
         "scheme")
        (t nil))
  "Program invoked by the `run-chez' command."
  :type 'string
  :group 'chez)

(defcustom% scheme-source? '(scheme-mode)
  "Used to determine if a buffer contains Scheme source code.
If it's loaded into a buffer that is in one of these major modes,
it's considered a scheme source file by `scheme-load-file' and
`scheme-compile-file'.  Used by these commands to determine
defaults."
  :type '(repeat function)
  :group 'chez)

(defcustom% chez-input-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'chez)

(defcustom% chez-repl-mode-hook nil
  "Hook run upon entry to `chez-repl-mode'.
This is run before the process is cranked up."
  :type 'hook
  :group 'chez)

(defvar *chez*
  (lexical-let% ((b))
    (lambda (&optional n)
      (if n (setq b n) b)))
  "The current *chez* process buffer.")

(defalias 'chez-switch-to-last-buffer
  (lexical-let% ((b))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (when b (switch-to-buffer-other-window b)))))
  "Switch to the last `chez-mode' buffer from `*chez*' buffer.")

(defvar *chez-start-file*
  (v-home% ".scheme/chez.ss")
  "The *chez* process start file.")

(defvar *chez-option-history*
  nil
  "Chez option history list.")

(defvar *chez-trace-history* nil
  "Chez tracing history list.")

(defvar chez-repl-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\C-c\C-a" #'chez-switch-to-last-buffer)
    m))

 ;; end variable declarations


(defun chez-input-filter (str)
  "Don't save anything matching `chez-input-filter-regexp'."
  (not (string-match chez-input-filter-regexp str)))

(defun chez-get-old-input ()
  "Snarf the sexp ending at point."
  (buffer-substring (save-excursion (backward-sexp)
                                    (point))
                    (point)))


(define-derived-mode chez-repl-mode comint-mode "REPL"
  "Major mode for interacting with a chez process.

The following commands are available:
\\{chez-repl-mode-map}

A chez process can be fired up with M-x `run-chez'.

Customization: 
Entry to this mode runs the hooks on `comint-mode-hook' and
  `chez-repl-mode-hook' (in that order).
You can send text to the chez process from other buffers
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
  (setq comint-prompt-regexp "^> *")
  (setq comint-prompt-read-only t)
  (setq comint-input-filter #'chez-input-filter)
  (setq comint-get-old-input #'chez-get-old-input)
  (when% (with-var byte-compile-warnings
           (setq byte-compile-warnings nil)
           (require 'scheme nil t))
    (require 'scheme)
    (scheme-mode-variables))
  (setq mode-line-process '(":%s")))


(defun run-chez (&optional command-line)
  "Run a chez process, input and output via buffer *chez*.

If there is a process already running in *chez*, switch to that
buffer.  With prefix COMMAND-LINE, allows you to edit the command
line (default is value of `chez-program').

Run the hook `chez-repl-mode-hook' after the `comint-mode-hook'."
  (interactive
   (list (if current-prefix-arg
             (read-string "Run chez: "
                          (car *chez-option-history*)
                          '*chez-option-history*
                          "--")
           "--")))
  (unless (comint-check-proc (funcall *chez*))
    (with-current-buffer (get-buffer-create "*chez*")
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             chez-program
             (when (file-exists-p *chez-start-file*)
               *chez-start-file*)
             (split-string* command-line "\\s-+" t))
      (funcall *chez* (current-buffer))
      (chez-repl-mode)))
  (switch-to-buffer-other-window "*chez*"))


 ;; end of REPL


(defun chez-proc ()
  "Return the `*chez*' process, starting one if necessary."
  (unless (comint-check-proc (funcall *chez*))
    (save-window-excursion
      (run-chez (read-string "Run chez: " (car *chez-option-history*)))))
  (or (get-buffer-process (funcall *chez*))
      (error "No current `*chez*' process.")))

(defun chez-switch-to-repl (&optional arg)
  "Switch to the `*chez*' buffer.

If ARG is non-nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (chez-proc)
  (chez-switch-to-last-buffer (current-buffer))
  (if arg
      ;; display REPL but do not select it
      (display-buffer (funcall *chez*)
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (funcall *chez*))
    (push-mark)
    (goto-char (point-max))))

(defun chez-compile-file (file)
  "Compile a Scheme FILE in `*chez*'."
  (interactive (comint-get-source
                "Compile Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source?
                nil)) 
  (comint-check-source file)
  (comint-send-string (chez-proc)
                      (format "(compile-file \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

(defun chez-load-file (file)
  "Load a Scheme FILE into `*chez*'."
  (interactive (comint-get-source
                "Load Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                scheme-source? t)) ;; t because `load'
  (comint-check-source file) 
  (comint-send-string (chez-proc)
                      (format "(load \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

(defun chez-send-region (start end)
  "Send the current region to `*chez*'."
  (interactive "r")
  (comint-send-region (chez-proc) start end)
  (comint-send-string (chez-proc) "\n")
  (chez-switch-to-repl t))

(defun chez-send-last-sexp ()
  "Send the previous sexp to `*chez*'."
  (interactive)
  (chez-send-region (save-excursion (backward-sexp)
                                      (point))
                      (point)))

(defun chez-send-definition ()
  "Send the current definition to `*chez*'."
  (interactive)
  (chez-send-region (save-excursion (beginning-of-defun)
                                    (point))
                    (save-excursion (end-of-defun)
                                    (point))))

(defun chez-trace-procedure (proc &optional untrace)
  "Trace or untrace procedure PROC in `*chez*' process.

If PROC is nil then untrace or list all traced procedures
determined by the prefix UNTRACE argument."
  (interactive (list (read-string (format "%s procedure: "
                                          (if current-prefix-arg
                                              "Untrace"
                                            "Trace"))
                                  (symbol-name (symbol-at-point))
                                  (car *chez-trace-history*))
                     current-prefix-arg))
  (comint-send-string (chez-proc)
                      (format "(%s %s)\n"
                              (if untrace "untrace" "trace")
                              proc))
  (chez-switch-to-repl t))

(defvar chez-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'chez-send-definition)
    (define-key m "\C-x\C-e" #'chez-send-last-sexp)
    (define-key m "\C-c\C-l" #'chez-load-file)
    (define-key m "\C-c\C-k" #'chez-compile-file)
    (define-key m "\C-c\C-r" #'chez-send-region)
    (define-key m "\C-c\C-t" #'chez-trace-procedure)
    (define-key m "\C-c\C-z" #'chez-switch-to-repl)
    ;; (scheme-mode-commands m)
    m))

(make-variable-buffer-local
 (defvar chez-mode-string nil
   "Modeline indicator for `chez-mode'."))

(defun chez-mode--lighter ()
  (or chez-mode-string
      (format " %s" (or "Chez" "C"))))

(define-minor-mode chez-mode
  "Toggle Chez's mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Chez mode is enabled, a host of nice utilities for
interacting with the Chez REPL is at your disposal.
\\{chez-mode-map}"
  :init-value nil
  :lighter (:eval (chez-mode--lighter))
  :group 'chez-mode
  :keymap chez-mode-map)




(provide 'chez)


;; end of chez.el
