;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; chez.el
;;;;
;;; https://scheme.com
;;; https://schemers.org
;;; https://cisco.github.io/ChezScheme/
;;; https://schemers.org/Documents/Standards/R5RS/HTML/
;;; https://www.emacswiki.org/emacs/r5rs.el
;;; https://groups.csail.mit.edu/mac/ftpdir/
;;;
;;; fetures:
;;; 1. start parameterized chez process.
;;; 2. switch/back to chez REPL.
;;; 3. send sexp/definition/region to chez REPL.
;;; 4. compile/load scheme file.
;;; 5. indentation in REPL.
;;; 6. completion in REPL and scheme source.
;;; 7. indentation for chez scheme.
;;;
;;; bugs:
;;;
;;;;;

(require 'comint)
(require 'scheme)
;; (require 'thingatpt)

;; ;; Disable `geiser-mode' for `scheme-mode'
;; (if-feature-geiser%
;;     (when-var% geiser-mode-auto-p 'geiser-mode
;;       (setq% geiser-mode-auto-p nil 'geiser-mode)))

;;;
;; variable declarations
;;;

(defgroup chez nil
  "Run a chez process in a buffer."
  :group 'scheme)


(defalias 'chez-program
  (lexical-let%
      ((b (executable-find%
           "scheme"
           (lambda (chez)
             (let ((x (shell-command* "echo"
                        "'(scheme-version)'|" chez "-q")))
               (or (and (zerop (car x)) chez)
                   "scheme"))))))
    (lambda (&optional n)
      "N"
      (if (null n) b (setq b n))))
  "Program invoked by the \\=`run-chez\\=' command.")


(defcustom% chez-input-filter-regexp "\\`\\s *\\S ?\\S ?\\s *\\'"
  "Input matching this regexp are not saved on the history list.
Defaults to a regexp ignoring all inputs of 0, 1, or 2 letters."
  :type 'regexp
  :group 'chez)

(defcustom% chez-repl-mode-hook nil
  "Hook run upon entry to \\=`chez-repl-mode'\\=.\n
This is run before the process is cranked up."
  :type 'hook
  :group 'chez)

(defalias '*chez*
  (lexical-let% ((b))
    (lambda (&optional n)
      "N"
      (cond (n (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*chez*")))
            (t b))))
  "The current *chez* process buffer.")


(defconst +chez-emacs-library+
  ";;; chez-emacs: from `(emacs-home* \"config/chez.el\")'
(library (chez-emacs)
    (export chez-emacs/apropos)
  (import (chezscheme))
  (define (chez-emacs/apropos what max)
    (let* ([lst (apropos-list what (interaction-environment))]
           [tbl (make-eq-hashtable)]
           [xs (let f ([ls lst] [acc tbl])
                 (cond [(null? ls) (hashtable-keys acc)]
                       [(symbol? (car ls))
                        (f (cdr ls)
                           (begin (hashtable-set! acc (car ls) '())
                                  acc))]
                       [else (f (cdar ls) acc)]))])
      (map symbol->string
           (if (> (vector-length xs) max)
               (let v->l ([n max] [ss '()])
                 (if (= n 0)
                     ss
                     (v->l (- n 1) (cons (vector-ref xs n) ss))))
               (vector->list xs))))))
(import (chez-emacs))
;;; eof
"
  "The library of chez-emacs.")


(defalias '*chez-out*
  (lexical-let% ((b "*out|chez*"))
    (lambda (&optional n)
      "N"
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of \\=`chez-completion'\\=.")


(defalias '*chez-start-file*
  (lexical-let% ((b (v-home% ".exec/chez.ss")))
    (lambda ()
      (cond ((file-exists-p b) b)
            (t (save-str-to-file +chez-emacs-library+ b)))))
  "The \\=`*chez*'\\= process start file.")


(defalias 'chez-switch-to-last-buffer
  (lexical-let% ((b nil))
    (lambda (&optional n)
      "N"
      (interactive "P")
      (cond (n (setq b n))
            (b (switch-to-buffer-other-window b)))))
  "Switch to the last \\=`chez-mode'\\= buffer from \\=`*chez*'\\=
buffer.")


(defvar *chez-option-history* nil
  "Chez option history list.")

(defvar *chez-trace-history* nil
  "Chez tracing history list.")


;; end variable declarations

;;;
;; proc
;;;

(defun chez-input-filter (str)
  "Don't save anything matching \\=`chez-input-filter-regexp'\\=."
  (not (string-match chez-input-filter-regexp str)))

(defun chez-get-old-input ()
  "Snarf the sexp ending at point."
  (let ((old (buffer-substring
              (save-excursion (backward-sexp) (point))
              (point))))
    (cond ((and (>= (length old) 2)
                (string= "> " (substring old 0 2)))
           (substring old 2))
          (t old))))

(defun chez-preoutput-filter (out)
  "Output start a newline when empty out or tracing."
  (cond ((string-match ".*\\(> \\)+\\{2,\\}$" out)
         (replace-match "\n> " t t out))
        ((and (>= (length out) 2)
              (string= "|" (substring out 0 1))
              (string= "" (chez-get-old-input)) "\n")
         (concat "\n" out))
        ((and (>= (length out) 2)
              (string= "> " (substring out 0 2))
              (string= "" (chez-get-old-input)) "\n")
         (concat "\n" out))
        (t out)))

(defun chez-check-proc (&optional spawn)
  "Return the `*chez*' process or start one if necessary."
  (when (and spawn
             (not (eq 'run (car (comint-check-proc (*chez*))))))
    (save-window-excursion (call-interactively #'run-chez)))
  (or (get-buffer-process (*chez*))
      (error "%s" "No *chez* process")))

(defun chez-input-complete-p ()
  "Return t if the input string contains a complete sexp."
  (save-excursion
    (let ((start (save-excursion
                   (comint-goto-process-mark) (point)))
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

(defun chez-completion ()
  (interactive)
  (chez-check-proc)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let ((cmd (format "(chez-emacs/apropos \"%s\" 64)"
                         (buffer-substring-no-properties
                          (car bounds)
                          (cdr bounds))))
            (proc (get-buffer-process (*chez*))))
        (with-current-buffer (*chez-out*)
          (erase-buffer)
          (comint-redirect-send-command-to-process
           cmd (*chez-out*) proc nil t)
          (set-buffer (*chez*))
          (while (or quit-flag (null comint-redirect-completed))
            (accept-process-output proc 2))
          (comint-redirect-cleanup)
          (setcar mode-line-process ""))
        (list (car bounds) (cdr bounds)
              (let ((s1 (read-from-string
                         (with-current-buffer (*chez-out*)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
                (when (and (consp s1) (consp (car s1)))
                  (car s1)))
              :exclusive 'no)))))

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
    (narrow-to-region (save-excursion
                        (comint-goto-process-mark) (point))
                      (point))
    (while (ignore-errors (save-excursion (backward-up-list 1)) t)
      (insert ")")))
  (chez-repl-return))


(defvar chez-repl-mode-map
  (let ((m (make-sparse-keymap "chez")))
    (if-graphic%
        (progn
          (define-key m [return] #'chez-repl-return)
          (define-key m [(control return)]
                      #'chez-repl-closing-return))
      (define-key m "\C-m" #'chez-repl-return)
      (define-key m "\C-\M-m" #'chez-repl-closing-return))
    (define-key m "\C-c\C-b" #'chez-switch-to-last-buffer)
    m)
  "The keymap for \\=`*chez*'\\= REPL.")


(defun chez-syntax-table ()
  "Specify special character in \\=`syntax-table'\\=."
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
A chez process can be fired up with M-x \\=`run-chez'\\=.
Customization:
Enter this mode runs the hooks on \\=`comint-mode-hook'\\= and
  \\=`chez-repl-mode-hook'\\= (in that order)."
  :group 'chez                          ; keyword args
  (setq comint-prompt-regexp "^[^>\n-\"]*>+ *")
  (setq comint-prompt-read-only t)
  (setq comint-input-filter #'chez-input-filter)
  (setq comint-get-old-input #'chez-get-old-input)
  (add-hook 'comint-preoutput-filter-functions
            #'chez-preoutput-filter nil 'local)
  (scheme-mode-variables)
  (use-local-map chez-repl-mode-map)
  (chez-syntax-table)
  (setq mode-line-process '("" ":%s")))


(defun run-chez (&optional command-line)
  "Run a chez process, input and output via buffer *chez*.\n
If there is a process already running in \\=`*chez*'\\=, switch
to that buffer. With prefix COMMAND-LINE, allows you to edit the
command line.
Run the hook \\=`chez-repl-mode-hook'\\= after the
\\=`comint-mode-hook'\\=."
  (interactive (list (read-string "Run chez: "
                                  (car *chez-option-history*)
                                  '*chez-option-history*)))
  (unless (comint-check-proc (*chez*))
    (unless (chez-program)
      (user-error "%s" "No chez program found"))
    (with-current-buffer (*chez*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             (chez-program)
             (*chez-start-file*)
             (split-string* command-line "\\s-+" t))
      (chez-repl-mode)
      (add-hook (if-var% completion-at-point-functions 'minibuffer
                         'completion-at-point-functions
                  'comint-dynamic-complete-functions)
                #'chez-completion 0 'local)))
  (switch-to-buffer-other-window (*chez*)))


(defun chez-switch-to-repl (&optional no-select)
  "Switch to the \\=`*chez*'\\= buffer.\n
If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (chez-check-proc t)
  (chez-switch-to-last-buffer (current-buffer))
  (if no-select
      (display-buffer (*chez*)
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*chez*))
    (push-mark)
    (goto-char (point-max))))

;; end of REPL


;;;
;; load/compile
;;;

(defun chez-compile-file (file)
  "Compile a Scheme FILE in \\=`*chez*'\\=."
  (interactive (comint-get-source
                "Compile Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(scheme-mode) nil))
  (comint-check-source file)
  (comint-send-string (chez-check-proc t)
                      (format "(compile-file \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

(defun chez-load-file (file)
  "Load a Scheme FILE into \\=`*chez*'\\=."
  (interactive (comint-get-source
                "Load Scheme file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(scheme-mode) nil))
  (comint-check-source file)
  (comint-send-string (chez-check-proc t)
                      (format "(load \"%s\")\n" file))
  (chez-switch-to-last-buffer (current-buffer))
  (chez-switch-to-repl))

;; end of load/compile

;;;
;; `chez-mode'
;;;

(defun chez-send-region (start end)
  "Send the current region to \\=`*chez*'\\=."
  (interactive "r")
  (process-send-region (chez-check-proc) start end)
  (comint-send-string (*chez*) "\n")
  (chez-switch-to-repl t))

(defun chez-send-last-sexp ()
  "Send the previous sexp to \\=`*chez*'\\=."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (chez-send-region (car bounds) (cdr bounds)))))

(defun chez-send-definition ()
  "Send the current definition to \\=`*chez*'\\=."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (chez-send-region (car bounds) (cdr bounds)))))

(defun chez-trace-procedure (proc &optional untrace)
  "Trace or untrace procedure PROC in \\=`*chez*'\\= process.\n
If PROC is nil then untrace or list all traced procedures
determined by the prefix UNTRACE argument."
  (interactive
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
  (comint-send-string (chez-check-proc)
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
    m))

(make-variable-buffer-local
 (defvar chez-mode-string nil
   "Modeline indicator for \\=`chez-mode'\\=."))

(defun chez-mode--lighter ()
  (or chez-mode-string " Chez"))

(define-minor-mode chez-mode
  "Toggle Chez's mode.\n
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.\n
When Chez mode is enabled, a host of nice utilities for
interacting with the Chez REPL is at your disposal.
\\{chez-mode-map}"
  :init-value nil
  :lighter (:eval (chez-mode--lighter))
  :group 'chez-mode
  :keymap chez-mode-map
  (add-hook (if-var% completion-at-point-functions 'minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'chez-completion 0 'local)
  (chez-syntax-indent))

;; end of `chez-mode'


(provide 'chez)


;; end of chez.el
