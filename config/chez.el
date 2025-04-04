;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; chez.el
;;;;
;; fetures:
;;; 1. start parameterized chez process.
;;; 2. switch/back to chez REPL.
;;; 3. send sexp/definition/region to chez REPL.
;;; 4. compile/load scheme file.
;;; 5. indentation in REPL and scheme buffer.
;;; 6. code completion in REPL and scheme buffer.
;;;;
;; references:
;;; https://scheme.com
;;; https://schemers.org
;;; https://cisco.github.io/ChezScheme/
;;; https://schemers.org/Documents/Standards/R5RS/HTML/
;;; https://www.emacswiki.org/emacs/r5rs.el
;;; https://groups.csail.mit.edu/mac/ftpdir/
;;;;


;;; require

(require 'comint)
(require 'scheme)

(eval-when-compile
  (require 'ed (v-home%> "config/ed")))

;; (require 'thingatpt)

;; end of require

;;;
;; chez environment
;;;

(defun chez-program-check ()
  "Check chez program path."
  (executable-find*
   "scheme"
   (lambda (chez)
     (let ((x (shell-command* "echo '(scheme-version)'|" chez "-q")))
       (and (zerop (car x)) chez)))))

(defalias 'chez-program
  (let ((b (chez-program-check)))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t b))))
  "Program invoked by the \\=`run-chez\\=' command.")

(defvar chez-repl-mode-hook nil
  "Hook run upon entry to \\=`chez-repl-mode\\='.\n
This is run before the process is cranked up.")

(defun *chez* ()
  "The current *chez* process buffer."
  (get-buffer-create "*chez*"))

(defun *chez-out* ()
  "The output buffer of \\=`chez-completion\\='."
  (get-buffer-create "*out|chez*"))

(defalias '*chez-start-file*
  (let ((b (v-home% ".exec/chez.ss")))
    (lambda ()
      (inhibit-file-name-handler
        (cond ((file-exists-p b) b)
              (t (copy-file (emacs-home% "config/chez.ss") b))))))
  "The \\=`*chez*\\=' process start file.")

(defalias 'chez-switch-to-last-buffer
  (let ((b nil))
    (lambda (&optional n)
      (interactive "P")
      (cond (n (setq b n))
            (b (switch-to-buffer-other-window b)))))
  "Switch to the last \\=`chez-mode\\=' buffer from \\=`*chez*\\=' buffer.")

(defvar *chez-option-history* nil
  "Chez option history list.")

(defvar *chez-trace-history* nil
  "Chez tracing history list.")

;; end of chez environment

;;;
;; proc
;;;

(defun chez-input-filter (in)
  "Don't save empty string in the history list."
  (null (string-match "\\`\\s *\\S ?\\S ?\\s *\\'" in)))

(defun chez-get-old-input ()
  "Snarf the sexp ending at point."
  (let ((old (buffer-substring
              (save-excursion (backward-sexp) (point)) (point))))
    (cond ((and (>= (length old) 2)
                (string-equal "> " (substring old 0 2)))
           (substring old 2))
          (t old))))

(defun chez-preoutput-filter (out)
  "Output start a newline when empty out or tracing."
  (cond ((string-match ".*\\(> \\)+\\{2,\\}$" out)
         (replace-match "\n> " t t out))
        ((and (>= (length out) 2)
              (string-equal "|" (substring out 0 1))
              (string-equal "" (chez-get-old-input)) "\n")
         (concat "\n" out))
        ((and (>= (length out) 2)
              (string-equal "> " (substring out 0 2))
              (string-equal "" (chez-get-old-input)) "\n")
         (concat "\n" out))
        (t out)))


(defun chez-check-proc (&optional spawn)
  "Return the \\=`*chez*\\=' process or start one if necessary."
  (when (and spawn (null (eq 'run (car (comint-check-proc (*chez*))))))
    (save-window-excursion (call-interactively #'run-chez)))
  (or (get-buffer-process (*chez*))
      (error "%s" "No *chez* process")))

(defun chez-input-complete-p ()
  "Return t if the input string contains a complete sexp."
  (save-excursion
    (let ((start (save-excursion (comint-goto-process-mark) (point)))
          (end (point-max)))
      (goto-char start)
      (cond ((looking-at "\\s *['`#]?[(\"]")
             (ignore-errors
               (save-restriction
                 (narrow-to-region start end)
                 (while (null (eobp))
                   (skip-chars-forward " \t\r\n)")
                   (forward-sexp))
                 t)))
            (t t)))))

(defun chez-completion-read (buffer)
  (car (read-from-string
        (with-current-buffer buffer
          (buffer-substring-no-properties
           (point-min) (point-max))))))

(defun chez-completion ()
  (interactive)
  (let* ((chez (*chez*)) (proc (get-buffer-process (*chez*)))
         (start nil) (end nil) (in nil))
    (when proc
      (with-current-buffer (current-buffer)
        (let ((bs (bounds-of-thing-at-point 'symbol)))
          (when bs
            (setq start (car bs)
                  end (cdr bs)
                  in (buffer-substring-no-properties start end)))))
      (when in
        (let ((cmd (format "(chez-emacs/apropos \"%s\" 1024)" in))
              (out (*chez-out*)))
          (with-current-buffer out (erase-buffer))
          (comint-redirect-send-command-to-process cmd out proc nil t)
          (with-current-buffer chez
            (unwind-protect
                (while (or quit-flag (null comint-redirect-completed))
                  (accept-process-output proc 2))
              (comint-redirect-cleanup)))
          (list start end (chez-completion-read out)
                :exclusive 'no))))))

;; end of proc

;;;
;; REPL
;;;

(defun chez-repl-return ()
  "Newline or indent then newline the current input."
  (interactive)
  (chez-check-proc)
  (cond ((chez-input-complete-p) (comint-send-input))
        (t (newline* 1))))

(defun chez-repl-closing-return ()
  "Close all open lists and evaluate the current input."
  (interactive)
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region
     (save-excursion (comint-goto-process-mark) (point))
     (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (chez-repl-return))

(defun chez-syntax-table ()
  "Specify special character in \\=`syntax-table\\='."
  (modify-syntax-entry ?| "_" (syntax-table)))

(defun chez-syntax-indent ()
  "Chez scheme syntax indent."
  (put 'library 'scheme-indent-function 2)
  (put 'trace-define 'scheme-indent-function 1)
  (put 'trace-do 'scheme-indent-function 1)
  (put 'trace-lambda 'scheme-indent-function 2)
  (put 'trace-let 'scheme-indent-function 2))

(define-derived-mode chez-repl-mode comint-mode "REPL"
  "Major mode for interacting with a chez process.\n
The following commands are available:
\\{chez-repl-mode-map}\n
A chez process can be fired up with M-x \\=`run-chez\\='.
Customization:
Enter this mode runs the hooks on \\=`comint-mode-hook\\=' and
  \\=`chez-repl-mode-hook\\=' (in that order)."
  :group 'chez                          ; keyword args
  (setq comint-prompt-regexp "^[^>\n-\"]*>+ *"
        comint-prompt-read-only t
        comint-input-filter #'chez-input-filter
        comint-get-old-input #'chez-get-old-input)
  (add-hook 'comint-preoutput-filter-functions
            #'chez-preoutput-filter nil 'local)
  (scheme-mode-variables)
  (chez-syntax-table)
  (setq mode-line-process '("" ":%s"))
  (let ((m (make-sparse-keymap "chez")))
    (set-keymap-parent m comint-mode-map)
    (if-graphic%
        (progn
          (define-key m [return] #'chez-repl-return)
          (define-key m [(control return)] #'chez-repl-closing-return))
      (define-key m "\C-m" #'chez-repl-return)
      (define-key m "\C-\M-m" #'chez-repl-closing-return))
    (define-key m "\C-c\C-b" #'chez-switch-to-last-buffer)
    (use-local-map m)))

(defun run-chez (&optional command-line)
  "Run a chez process, input and output via buffer *chez*.\n
If there is a process already running in \\=`*chez*\\=', switch
to that buffer. With prefix COMMAND-LINE, allows you to edit the
command line.
Run the hook \\=`chez-repl-mode-hook\\=' after the \\=`comint-mode-hook\\='."
  (interactive (read-string-prompt "Run chez: " '*chez-option-history*))
  (unless (comint-check-proc (*chez*))
    (unless (chez-program)
      (error "%s" "No chez program found"))
    (with-current-buffer (*chez*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             (chez-program)
             (*chez-start-file*)
             (cdr (split-string* command-line "\\s-+" t)))
      (chez-repl-mode)
      (add-hook (if-var% completion-at-point-functions minibuffer
                         'completion-at-point-functions
                  'comint-dynamic-complete-functions)
                #'chez-completion 0 'local)))
  (switch-to-buffer-other-window (*chez*)))

(defun chez-switch-to-repl (&optional no-select)
  "Switch to the \\=`*chez*\\=' buffer.\n
If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (chez-check-proc t)
  (chez-switch-to-last-buffer (current-buffer))
  (if no-select
      (display-buffer (*chez*)
                      (if-fn% display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*chez*))
    (push-mark)
    (goto-char (point-max))))

;; end of REPL

;;;
;; compile / load
;;;

(defun chez--file-prompt (prompt)
  (comint-get-source
   prompt
   (let ((n (buffer-file-name)))
     (cons (file-name-directory n) (file-name-nondirectory n)))
   '(scheme-mode) nil))

(defun chez-compile-file (file)
  "Compile a Scheme FILE in \\=`*chez*\\='."
  (interactive (chez--file-prompt "Compile Scheme file: "))
  (comint-check-source file)
  (comint-send-string (chez-check-proc t)
                      (format "(compile-file \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

(defun chez-load-file (file)
  "Load a Scheme FILE into \\=`*chez*\\='."
  (interactive (chez--file-prompt "Load Scheme file: "))
  (comint-check-source file)
  (comint-send-string (chez-check-proc t) (format "(load \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

;; end of compile / load

;;;
;; `chez-mode'
;;;

(defun chez-send-region (start end)
  "Send the current region to \\=`*chez*\\='."
  (interactive "r")
  (process-send-region (chez-check-proc) start end)
  (comint-send-string (*chez*) "\n")
  (chez-switch-to-repl t))

(defun chez-send-last-sexp ()
  "Send the previous sexp to \\=`*chez*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (chez-send-region (car bounds) (cdr bounds)))))

(defun chez-send-definition ()
  "Send the current definition to \\=`*chez*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (chez-send-region (car bounds) (cdr bounds)))))

(defun chez--trace-procedure-prompt ()
  (list (read-string (format "%s procedure: "
                             (if current-prefix-arg
                                 "Untrace"
                               "Trace"))
                     (if current-prefix-arg
                         (car *chez-trace-history*)
                       (when (symbol-at-point)
                         (symbol-name (symbol-at-point))))
                     '*chez-trace-history*)
        current-prefix-arg))

(defun chez-trace-procedure (proc &optional untrace)
  "Trace or untrace procedure PROC in \\=`*chez*\\=' process.\n
If PROC is nil then untrace or list all traced procedures
determined by the prefix UNTRACE argument."
  (interactive (chez--trace-procedure-prompt))
  (comint-send-string
   (chez-check-proc)
   (format "(%s %s)\n" (if untrace "untrace" "trace") proc))
  (chez-switch-to-repl t))

(make-variable-buffer-local
 (defvar chez-mode-string nil
   "Modeline indicator for \\=`chez-mode\\='."))

(defun chez--mode-keymap ()
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-c\C-k" #'chez-compile-file)
    (define-key keymap "\C-c\C-l" #'chez-load-file)
    (define-key keymap "\C-c\C-r" #'chez-send-region)
    (define-key keymap "\C-c\C-t" #'chez-trace-procedure)
    (define-key keymap "\C-c\C-z" #'chez-switch-to-repl)
    (define-key keymap "\C-x\C-e" #'chez-send-last-sexp)
    (define-key keymap "\M-\C-x" #'chez-send-definition)
    keymap))

(defvar chez-mode-map (chez--mode-keymap)
  "The keymap of \\=`chez-mode\\='.")

(define-minor-mode chez-mode
  "Toggle Chez's mode.\n
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.\n
When Chez mode is enabled, a host of nice utilities for
interacting with the Chez REPL is at your disposal.
\\{chez-mode-map}"
  :init-value nil
  :lighter " Chez"
  :keymap chez-mode-map
  :group 'scheme
  (add-hook (if-var% completion-at-point-functions minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'chez-completion 0 'local)
  (chez-syntax-indent))

;; end of `chez-mode'


(provide 'chez)


;; end of chez.el
