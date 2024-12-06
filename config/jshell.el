;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; jshell.el
;;;;
;; fetures:
;;; 1. start parameterized jshell process.
;;; 2. switch/back to jshell REPL.
;;; 3. send sexp/definition/region to jshell REPL.
;;; 4. completion in REPL and java source.
;;;;
;; references:
;;; https://docs.oracle.com/en/java/javase/21/jshell/introduction-jshell.html
;;;;
;;; bugs:
;;;
;;;;;

;;; require

(require 'comint)

;; end of require

;;;
;; jshell environtment
;;;

(defgroup jshell nil
  "Run a jshell process in a buffer."
  :group 'jshell)

(defalias 'jshell-program
  (lexical-let% ((b (executable-find%
                     "jshell"
                     (lambda (jshell)
                       (let ((x (shell-command* jshell "--version")))
                         (or (and (zerop (car x)) jshell)
                             "jshell"))))))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t b))))
  "Program invoked by the \\=`run-jshell\\=' command.")

(defalias '*jshell*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond (n (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*jshell*")))
            (t b))))
  "The current *jshell* process buffer.")

(defalias '*jshell-out*
  (lexical-let% ((b "*out|jshell*"))
    (lambda (&optional n)
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of \\=`jshell-completion\\='.")

(defalias '*jshell-start-file*
  (lexical-let% ((b (v-home% ".exec/jshell.jsh")))
    (lambda ()
      (cond ((file-exists-p b) b)
            (t (copy-file (emacs-home% "config/jshell.jsh") b)))))
  "The \\=`*jshell*\\=' process start file.")

(defalias 'jshell-switch-to-last-buffer
  (lexical-let% ((b))
    (lambda (&optional n)
      (interactive "P")
      (cond (n (setq b n))
            (b (switch-to-buffer-other-window b)))))
  "Switch to the last \\=`jshell-mode\\=' buffer from \\=`*jshell*\\=' buffer.")

(defvar *jshell-option-history* nil
  "Jshell option history list.")

;; end of jshell environment

;;;
;; proc
;;;

(defun jshell-input-filter (in)
  ;; raw
  in)

(defun jshell-get-old-input ()
  "Snarf the sexp ending at point."
  (buffer-substring (point-min) (point-max)))

(defun jshell-preoutput-filter (out)
  "Output start a newline when empty out or tracing."
  ;; raw
  out)

(defun jshell-check-proc (&optional spawn)
  "Return the \\`*jshell*\\=' process or start one if necessary."
  (when (and spawn (not (eq 'run (car (comint-check-proc (*jshell*))))))
    (save-window-excursion (call-interactively #'run-jshell)))
  (or (get-buffer-process (*jshell*))
      (error "%s" "No *jshell* process")))

(defun jshell-last-symbol ()
  "Return the position of left side of the last symbol."
  (save-excursion
    (catch 'br
      (while (not (or (char= (char-before) ?\;)
                      (char= (char-before) ?\n)
                      (eq (char-syntax (char-before)) ? )))
        (cond ((or (char= (char-before) ?.)
                   (char= (char-before) ?_))
               (backward-char))
              ((eq (char-syntax (char-before)) ?w)
               (backward-word))
              (t (throw 'br (point))))))
    (point)))

(defun jshell-completion-read (in buffer)
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((ss nil) (in1 (concat in "[._a-zA-Z0-9]+")))
      (while (search-forward-regexp in1 nil t)
        (let ((sym (buffer-substring-no-properties
                    (match-beginning 0) (match-end 0))))
          (setq ss (append! sym ss t))))
      ss)))

(defun jshell-completion ()
  (interactive)
  (let* ((jshell (*jshell*)) (proc (get-buffer-process jshell))
         (sym nil) (last nil))
    (when proc
      (with-current-buffer jshell
        (setq sym (bounds-of-thing-at-point 'symbol)
              last (cons (jshell-last-symbol) (point))))
      (when (and sym last)
        (let* ((tkn (buffer-substring-no-properties (car sym) (cdr sym)))
               (sxp (buffer-substring-no-properties (car last) (cdr last)))
               ;; tricky, the last forward slash make error no wait.
               (cmd (format "%s\t\t\\" sxp))
               (out (*jshell-out*)))
          (with-current-buffer out (erase-buffer))
          (comint-redirect-send-command-to-process cmd out proc nil t)
          (with-current-buffer jshell
            (unwind-protect
                (while (or quit-flag (null comint-redirect-completed))
                  (accept-process-output proc 2))
              (comint-redirect-cleanup)))
          (list (car sym) (cdr sym) (jshell-completion-read tkn out)
                :exclusive 'no))))))

;; end of proc

;;;
;; REPL
;;;

(defvar jshell-repl-mode-map
  (let ((m (make-sparse-keymap "jshell")))
    (define-key m "\C-c\C-b" #'jshell-switch-to-last-buffer)
    m)
  "The keymap for \\=`*jshell*\\=' REPL.")

(define-derived-mode jshell-repl-mode comint-mode "REPL"
  "Major mode for interacting with a jshell process.\n
The following commands are available:
\\{jshell-repl-mode-map}
A jshell process can be fired up with M-x \\=`run-jshell\\='.\n
Customization:
Entry to this mode runs the hooks on \\=`comint-mode-hook\\=' and
  \\=`jshell-repl-mode-hook\\=' (in that order)."
  :group 'jshell                        ; keyword args
  (setq comint-prompt-regexp "^\\(?:jshell> *\\)"
        comint-prompt-read-only t
        comint-input-filter #'jshell-input-filter
        comint-get-old-input #'jshell-get-old-input
        mode-line-process '("" ":%s"))
  (add-hook 'comint-preoutput-filter-functions
            #'jshell-preoutput-filter nil 'local)
  (use-local-map jshell-repl-mode-map))

(defun run-jshell (&optional command-line)
  "Run a jshell process, input and output via buffer *jshell*.\n
If there is a process already running in \\=`*jshell*\\=', switch to that
buffer. With prefix COMMAND-LINE, allows you to edit the command line.\n
Run the hook \\=`jshell-repl-mode-hook\\=' after the \\=`comint-mode-hook\\='."
  (interactive (list (read-string "Run jshell: "
                                  (car *jshell-option-history*)
                                  '*jshell-option-history*)))
  (unless (comint-check-proc (*jshell*))
    (unless (jshell-program)
      (error "%s" "No jshell program found"))
    (with-current-buffer (*jshell*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             (jshell-program)
             nil
             (append (list "--startup" "DEFAULT"
                           "--startup" "PRINTING"
                           "--startup" (*jshell-start-file*))
                     (split-string* command-line "\\s-+" t)))
      (jshell-repl-mode)
      (add-hook (if-var% completion-at-point-functions 'minibuffer
                         'completion-at-point-functions
                  'comint-dynamic-complete-functions)
                #'jshell-completion 0 'local)))
  (switch-to-buffer-other-window (*jshell*)))

(defun jshell-switch-to-repl (&optional no-select)
  "Switch to the `*jshell*' buffer.\n
If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (jshell-check-proc t)
  (jshell-switch-to-last-buffer (current-buffer))
  (if no-select
      (display-buffer (*jshell*)
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*jshell*))
    (push-mark)
    (goto-char (point-max))))

;; end of REPL

;;;
;; load
;;;

(defun jshell-load-file (file)
  "Load a java FILE into \\=`*jshell*\\='."
  (interactive (comint-get-source "Load java file: "
                                  (let ((n (buffer-file-name)))
                                    (cons (file-name-directory n)
                                          (file-name-nondirectory n)))
                                  '(java-mode) nil))
  (comint-check-source file)
  (comint-send-string (jshell-check-proc t)
                      (format "/open %s%s\n"
                              (file-name-directory file)
                              (file-name-nondirectory file)))
  (jshell-switch-to-last-buffer (current-buffer))
  (jshell-switch-to-repl))

;; end of load

;;;
;; `jshell-mode'
;;;

(defun jshell-send-region (start end)
  "Send the current region to \\=`*jshell*\\='."
  (interactive "r")
  (process-send-string
   (jshell-check-proc)
   (replace-regexp-in-string "^\t+" " " (buffer-substring start end)))
  (comint-send-string (*jshell*) "\n")
  (jshell-switch-to-repl t))

(defun jshell-send-definition ()
  "Send the current definition to \\=`*jshell*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (jshell-send-region (car bounds) (cdr bounds)))))

(defun jshell-send-line ()
  "Send the current line to \\=`*jshell*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (jshell-send-region (car bounds) (cdr bounds)))))

(defun jshell-inspect-object ()
  "Inspect object."
  (interactive)
  (let ((bounds (if-region-active
                    (cons (region-beginning) (region-end))
                  (let ((b (jshell-last-symbol))
                        (s (bounds-of-thing-at-point 'symbol)))
                    (cond ((and b s) (cons (cond ((<= b (car s)) b)
                                                 (t (car s)))
                                           (cond ((>= (cdr s) (point))
                                                  (cdr s))
                                                 (t b))))
                          (b (cons (cond ((<= b (point)) b)
                                         (t (point)))
                                   (point)))
                          (s (cons (cond ((<= (car s) (point)) (car s))
                                         (t (point)))
                                   (point))))))))
    (when bounds
      (let ((lhs (car bounds))
            (rhs (cdr bounds)))
        (when (and (eq (char-syntax (char-before lhs)) ?\")
                   (eq (char-syntax (char-after rhs)) ?\"))
          (setq lhs (1- (car bounds))
                rhs (1+ (cdr bounds))))
        (jshell-send-region lhs rhs)))))

(defvar jshell-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'jshell-send-definition)
    (define-key m "\C-c\C-j" #'jshell-send-line)
    (define-key m "\C-c\C-l" #'jshell-load-file)
    ;; (define-key m "\C-c\C-k" #'jshell-compile-file)
    (define-key m "\C-c\C-r" #'jshell-send-region)
    (define-key m "\C-c\C-z" #'jshell-switch-to-repl)
    (define-key m "\C-cI" #'jshell-inspect-object)
    m))

(make-variable-buffer-local
 (defvar jshell-mode-string nil
   "Modeline indicator for \\=`jshell-mode\\='."))

(defun jshell-mode--lighter ()
  (or jshell-mode-string " Jshell"))

(defun jshell-syntax-indent ()
  "Jshell java syntax indent.")

(define-minor-mode jshell-mode
  "Toggle Jshell's mode.\n
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.\n
When Jshell mode is enabled, a host of nice utilities for
interacting with the Jshell REPL is at your disposal.
\\{jshell-mode-map}"
  :init-value nil
  :lighter (:eval (jshell-mode--lighter))
  :group 'jshell-mode
  :keymap jshell-mode-map
  (add-hook (if-var% completion-at-point-functions 'minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'jshell-completion 0 'local)
  (jshell-syntax-indent))

;; end of jshell-mode


(provide 'jshell)


;; end of jshell.el
