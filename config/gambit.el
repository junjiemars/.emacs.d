;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; gambit.el
;;;;
;;; features:
;;; 1. start parameterized gambit process.
;;; 2. switch/back to gambit REPL.
;;; 3. send sexp/definition/region to gambit REPL.
;;; 4. compile/load scheme file.
;;; 5. indentation in REPL.
;;; 6. completion in REPL and scheme source.
;;; 7*. migrate features from `(emacs-home* "config/chez.el")'.
;;; 
;;; bugs: 
;;;
;;; improve:
;;; 1. enlarge comint redirect buffered size.
;;; 


(require 'comint)
(require 'scheme)
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


(defconst +gambit-emacs-library+
  ";;; gambit-emacs: from `(emacs-home* \"config/gambit.el\")'
(define (gambit-emacs/apropos what)
	(let* ([split (lambda (ss sep)
									(call-with-input-string
											ss
										(lambda (p)
											(read-all p (lambda (p)
																		(read-line p sep))))))]
				 [trim (lambda (ss)
								 (letrec ([tr (lambda (ss ori idx)
																(let ([a (if (> (string-length ss) 0)
																						 (substring ss 0 1)
																						 \"\")]
																			[l (string-length ss)]
																			[ns \"namespace\"]
																			[empty \"empty\"]
																			[cs \"c#\"]
																			[asm \"_asm#\"]
                                      [syn \"syn#\"]
                                      [x86 \"_x86#\"])
																	(cond [(string=? \"\" a) a]
																				[(or (string=? a \" \")
																						 (string=? a \":\")
																						 (string=? a \"\\n\")
																						 (string=? a \"#\")
																						 (string=? a \"\\\"\"))
																				 (tr (substring ss 1 l) ori (+ idx 1))]
																				[(and (>= l (string-length ns))
																							(string=? (substring ss 0 (string-length ns))
																												ns))
																				 (tr (substring ss (string-length ns) (string-length ss))
																						 ori (+ idx (string-length ns)))]
																				[(and (>= l (string-length empty))
																							(string=? (substring ss 0 (string-length empty))
																												empty))
																				 (tr (substring ss (string-length empty) (string-length ss))
																						 ori (+ idx (string-length empty)))]
                                        [(and (>= l (string-length cs))
																							(string=? (substring ss 0 (string-length cs))
																												cs))
																				 (tr (substring ss (string-length cs) (string-length ss))
																						 ori (+ idx (string-length cs)))]
																				[(and (>= l (string-length asm))
																							(string=? (substring ss 0 (string-length asm))
																												asm))
																				 (tr (substring ss (string-length asm) (string-length ss))
																						 ori (+ idx (string-length asm)))]
                                        [(and (>= l (string-length syn))
																							(string=? (substring ss 0 (string-length syn))
																												syn))
																				 (tr (substring ss (string-length syn) (string-length ss))
																						 ori (+ idx (string-length syn)))]
                                        [(and (>= l (string-length x86))
																							(string=? (substring ss 0 (string-length x86))
																												x86))
																				 (tr (substring ss (string-length syn) (string-length ss))
																						 ori (+ idx (string-length x86)))]
																				[else (substring ori idx (string-length ori))])))])
									 (tr ss ss 0)))]
				 [lst (let ([out (open-output-string)])
								(apropos what out)
								(map (lambda (x)
											 (when (not (string=? \"\" x))
												 (trim x)))
										 (split (get-output-string out) #\\,)))]
				 [tbl (make-table 'test: string=?)])
		(map (lambda (x)
					 (when (not (null? x))
						 (map (lambda (y)
										(unless (string=? \"\" y)
											(table-set! tbl y '())))
									(let f ([ss (split x #\\newline)] [acc '()])
										(cond [(null? ss) acc]
													[(null? (car ss)) (f (cdr ss) acc)]
													[(string=? \"\" (car ss)) (f (cdr ss) acc)]
													[else (f (cdr ss) (cons (trim (car ss)) acc))])))))
				 lst)
		(let ([out (map car (table->list tbl))])
			(if (> (length out) 10)
					(let f ([xs out] [n 10] [acc '()])
						(if (= n 0)
								acc
								(f (cdr xs) (- n 1) (cons (car xs) acc))))
					out))))

;;; eof
 " 
  "The library of gambit-emacs.")


(defalias '*gambit-out*
  (lexical-let% ((b "*out|gambit*"))
    (lambda (&optional n)
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of `gambit-completion'.")


(defalias '*gambit-start-file*
  (lexical-let% ((b (v-home% ".exec/.gambit.ss")))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (unless (file-exists-p b)
          (save-str-to-file +gambit-emacs-library+ b))
        b)))
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
  (let ((old (buffer-substring (save-excursion (backward-sexp) (point))
                             (point))))
    (cond ((and (>= (length old) 2)
                (string= "> " (substring old 0 2)))
           (substring old 2))
          (t old))))

(defun gambit-preoutput-filter (out)
  "Output start a newline when empty out or tracing."
  (cond ((string-match ".*\\(> \\)+\\{2,\\}$" out)
         (replace-match "\n> " t t out))
        ((and (>= (length out) 2)
              (string= "|" (substring out 0 1))
              (string= "" (gambit-get-old-input)) "\n")
         (concat "\n" out))
        ((and (>= (length out) 2)
              (string= "> " (substring out 0 2))
              (string= "" (gambit-get-old-input)) "\n")
         (concat "\n" out))
        (t out)))


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


(defun gambit-completion ()
  "Return `*gambit*' completion list."
  (interactive)
  (gambit-check-proc)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let ((cmd (format "(gambit-emacs/apropos \"%s\")"
                         (buffer-substring-no-properties (car bounds)
                                                         (cdr bounds))))
            (proc (get-buffer-process (*gambit*))))
        (with-current-buffer (*gambit-out*)
          (erase-buffer)
          (comint-redirect-send-command-to-process cmd
                                                   (*gambit-out*)
                                                   proc nil t)
          (set-buffer (*gambit*))
          (while (and (null comint-redirect-completed)
                      (accept-process-output proc))))
        (list (car bounds) (cdr bounds)
              (let ((s1 (read-from-string
                         (with-current-buffer (*gambit-out*)
                           (buffer-substring-no-properties
                            (point-min) (point-max))))))
                (when (consp s1) (car s1)))
              :exclusive 'no)))))


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
  (let ((m (make-sparse-keymap "gambit")))
    (if-graphic%
        (progn
          (define-key m [return] #'gambit-repl-return)
          (define-key m [(control return)] #'gambit-repl-closing-return))
      (define-key m "\C-m" #'gambit-repl-return)
      (define-key m "\C-\M-m" #'gambit-repl-closing-return))
    (define-key m "\C-c\C-b" #'gambit-switch-to-last-buffer)
    m)
  "The keymap for `*gambit*' REPL.")


(defun gambit-syntax-indent ()
  "Gambit scheme syntax indent.")


(define-derived-mode gambit-repl-mode comint-mode "REPL"
  "Major mode for interacting with a gambit process.

The following commands are available:
\\{gambit-repl-mode-map}

A gambit process can be fired up with M-x `run-gambit'.

Customization: 
Entry to this mode runs the hooks on `comint-mode-hook' and
  `gambit-repl-mode-hook' (in that order)."
  :group 'gambit                          ; keyword args
  (setq comint-prompt-regexp "^[^>\n-\"]*>+ *")
  (setq comint-prompt-read-only t)
  (setq comint-input-filter #'gambit-input-filter)
  (setq comint-get-old-input #'gambit-get-old-input)
  (add-hook 'comint-preoutput-filter-functions
            #'gambit-preoutput-filter nil 'local)
  (scheme-mode-variables)
  (use-local-map gambit-repl-mode-map)
  (setq mode-line-process '(":%s")))


(defun run-gambit (&optional command-line)
  "Run a gambit process, input and output via buffer *gambit*.

If there is a process already running in `*gambit*', switch to that
buffer. With prefix COMMAND-LINE, allows you to edit the command
line.

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
      (gambit-repl-mode)
      (add-hook (if-var% completion-at-point-functions 'minibuffer
                         'completion-at-point-functions
                  'comint-dynamic-complete-functions)
                #'gambit-completion 0 'local)))
  (switch-to-buffer-other-window (*gambit*)))


 ;; end of REPL


(defun gambit-switch-to-repl (&optional no-select)
  "Switch to the `*gambit*' buffer.

If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (gambit-check-proc t)
  (gambit-switch-to-last-buffer (current-buffer))
  (if no-select
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
  (interactive (comint-get-source "Compile Scheme file: "
                                  (let ((n (buffer-file-name)))
                                    (cons (file-name-directory n)
                                          (file-name-nondirectory n)))
                                  '(scheme-mode)
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
                '(scheme-mode) nil))
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
  :keymap gambit-mode-map
  (add-hook (if-var% completion-at-point-functions 'minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'gambit-completion 0 'local)
  (gambit-syntax-indent))



(provide 'gambit)


;; end of gambit.el
