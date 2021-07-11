;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; jshell.el
;;;;
;;; https://nodejs.org
;;;
;;; fetures:
;;; 1. start parameterized jshell process.
;;; 2. switch/back to jshell REPL.
;;; 3. send sexp/definition/region to jshell REPL.
;;; 4. TODO: completion in REPL and java source.
;;;
;;; bugs:
;;;
;;;;;

(require 'comint)
;; (require 'thingatpt)



;; variable declarations

(defgroup jshell nil
  "Run a jshell process in a buffer."
  :group 'jshell)


(defalias 'jshell-program
  (lexical-let% ((b (executable-find%
                     "jshell"
                     (lambda (jshell)
                       (let ((x (shell-command* jshell "--version")))
                         (zerop (car x)))))))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program invoked by the `run-jshell' command.")


(defalias '*jshell*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond ((not (null n))
             (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*jshell*")))
            (t b))))
  "The current *jshell* process buffer.")


(defconst +jshell-emacs-module+
  "
void jshell_emacs_apropos(String what, int max) {
  System.out.println(what);
}
"
  "The module of jshell-emacs.")


(defalias '*jshell-out*
  (lexical-let% ((b "*out|jshell*"))
    (lambda (&optional n)
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of `jshell-completion'.")


(defalias '*jshell-start-file*
  (lexical-let% ((b (v-home% ".exec/jshell.jsh")))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (unless (file-exists-p b)
          (save-str-to-file +jshell-emacs-module+ b))
        b)))
  "The `*jshell*' process start file.")


(defalias 'jshell-switch-to-last-buffer
  (lexical-let% ((b))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (when b (switch-to-buffer-other-window b)))))
  "Switch to the last `jshell-mode' buffer from `*jshell*' buffer.")


(defvar *jshell-option-history* nil
  "Jshell option history list.")


 ;; end variable declarations


(defun jshell-check-proc (&optional spawn)
  "Return the `*jshell*' process or start one if necessary."
  (when (and spawn
             (not (eq 'run (car (comint-check-proc (*jshell*))))))
    (save-window-excursion (call-interactively #'run-jshell)))
  (or (get-buffer-process (*jshell*))
      (error "No `*jshell*' process.")))


(defun jshell-last-sexp ()
  "Return the position of left side of the last expression."
  (save-excursion
    (catch 'break
      (let ((ori (point)))
        (while (not (or (= (point) (point-min))
                        (char= (char-before) ?\;)
                        (char= (char-before) ?\n)))
          (cond
           ;; right parenthesis
           ((char= (char-before) ?\))
            (backward-list))
           ;; word
           ((char= (char-syntax (char-before)) ?w)
            (let ((cur (point))
                  (idx 1))
              (when (<= (- cur idx) (point-min))
                (throw 'break (point-min)))
              (while (char= (char-syntax (char-before (- cur idx))) ?w)
                (setq idx (1+ idx)))
              (when (and (> idx 2)
                         (string-match
                          "var"
                          (buffer-substring-no-properties
                           (- cur idx) cur)))
                (throw 'break cur))
              (backward-word)))
           ;; whitespace
           ((char= (char-syntax (char-before)) ?\ )
            (while (char= (char-syntax (char-before)) ?\ )
              (backward-char)))
           ;; comma
           ((char= (char-before) ?,)
            (throw 'break (point)))
           ;; assignment
           ((char= (char-before) ?=)
            (let ((cur (point))
                  (idx 1))
              (when (<= (- cur idx) (point-min))
                (throw 'break (point-min)))
              (while (or (char= (char-before (- cur idx)) ?=)
                         (char= (char-before (- cur idx)) ?!))
                (setq idx (1+ idx)))
              (if (< idx 2)
                  (throw 'break cur)
                (backward-char idx))))
           ;; > >> >>>
           ((char= (char-before) ?>)
            (when (string-match
                   "^[> \t]+"
                   (buffer-substring-no-properties
                    (point-at-bol)
                    (point)))
              (throw 'break (point)))
            (backward-char))
           ;; dot
           ((char= (char-before) ?.)
            (let ((cur (point))
                  (idx 1))
              (when (<= (- cur idx) (point-min))
                (throw 'break (point-min)))
              (while (char= (char-before (- cur idx)) ?.)
                (setq idx (1+ idx)))
              (when (>= idx 3)
                (throw 'break cur))
              (backward-char)))
           ;; punctuation
           ((char= (char-syntax (char-before)) ?.)
            (let ((cur (point)))
              (when (or (= ori cur)
                        (and (< cur ori)
                             (let ((idx 0))
                               (while (and (< (+ cur idx) ori)
                                           (char= (char-syntax
                                                   (char-after (+ cur idx)))
                                                  ?\ ))
                                 (setq idx (1+ idx)))
                               (= (+ cur idx) ori))))
                (throw 'break ori))
              (backward-char)))
           ;; default
           (t (throw 'break (point)))))))
    (while (char= (char-syntax (char-after)) ?\ )
      (forward-char))
    (point)))

(defun jshell-last-symbol ()
  "Return the position of left side of the last symbol."
  (save-excursion
    (catch 'break
      (while (not (or (char= (char-before) ?\;)
                      (char= (char-before) ?\n)
                      (eq (char-syntax (char-before)) ? )))
        (cond ((or (char= (char-before) ?.)
                   (char= (char-before) ?_))
               (backward-char))
              ((eq (char-syntax (char-before)) ?w)
               (backward-word))
              (t (throw 'break (point))))))
    (point)))


(defun jshell-completion ()
  (interactive)
  (jshell-check-proc)
  (let ((bounds (cons (save-excursion (jshell-last-symbol))
                      (point))))
    (if (= (car bounds) (cdr bounds))
        (list (car bounds) (cdr bounds) :exclusive 'no)
      (let ((cmd (format "jshell_emacs_apropos(\"%s\", 64)"
                         (buffer-substring-no-properties (car bounds)
                                                         (cdr bounds))))
            (proc (get-buffer-process (*jshell*))))
        (with-current-buffer (*jshell-out*)
          (erase-buffer)
          (comint-redirect-send-command-to-process cmd
                                                   (*jshell-out*)
                                                   proc nil t)
          (set-buffer (*jshell*))
          (while (or quit-flag (null comint-redirect-completed))
            (accept-process-output proc 2))
          (comint-redirect-cleanup)
          (setcar mode-line-process ""))
        (list (car bounds) (cdr bounds)
              (let ((s1 (read-from-string
                         (with-current-buffer (*jshell-out*)
                           (string-trim><
                            (buffer-substring-no-properties
                             (point-min) (point-max))
                            "^undefined.*\\|[ \t\n]*\\'"
                            "[ \t\n]*jshell_emacs_apropos.*")))))
                (when (and (consp s1) (consp (car s1)))
                  (car s1)))
              :exclusive 'no)))))


(defvar jshell-repl-mode-map
  (let ((m (make-sparse-keymap "jshell")))
    (define-key m "\C-c\C-b" #'jshell-switch-to-last-buffer)
    m)
  "The keymap for `*jshell*' REPL.")


(define-derived-mode jshell-repl-mode comint-mode "REPL"
  "Major mode for interacting with a jshell process.

The following commands are available:
\\{jshell-repl-mode-map}

A jshell process can be fired up with M-x `run-jshell'.

Customization:
Entry to this mode runs the hooks on `comint-mode-hook' and
  `jshell-repl-mode-hook' (in that order)."
  :group 'jshell                          ; keyword args
  (setq comint-prompt-regexp "^\\(?:jshell> *\\)")
  (setq comint-prompt-read-only t)
  (use-local-map jshell-repl-mode-map)
  (setq mode-line-process '("" ":%s")))


(defun run-jshell (&optional command-line)
  "Run a jshell process, input and output via buffer *jshell*.

If there is a process already running in `*jshell*', switch to that
buffer. With prefix COMMAND-LINE, allows you to edit the command
line.

Run the hook `jshell-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (list (read-string "Run jshell: "
                                  (car *jshell-option-history*)
                                  '*jshell-option-history*)))
  (unless (comint-check-proc (*jshell*))
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
  "Switch to the `*jshell*' buffer.

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


(defun jshell-compile-file (file)
  "Compile a javascript FILE in `*jshell*'."
  (interactive (comint-get-source
                "Compile java file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(js-mode) nil))
  (comint-check-source file)
  (comint-send-string (jshell-check-proc t)
                      (format "System.out.printf('\"%s\"')\n" file))
  (jshell-switch-to-last-buffer (current-buffer))
  (jshell-switch-to-repl))

(defun jshell-load-file (file)
  "Load a javascript FILE into `*jshell*'."
  (interactive (comint-get-source
                "Load javascript file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(js-mode) nil))
  (comint-check-source file)
  (comint-send-string (jshell-check-proc t)
                      (format "process.chdir('%s');\n.load %s\n"
                              (file-name-directory file)
                              (file-name-nondirectory file)))
  (jshell-switch-to-last-buffer (current-buffer))
  (jshell-switch-to-repl))


(defun jshell-send-region (start end)
  "Send the current region to `*jshell*'."
  (interactive "r")
  (process-send-region (jshell-check-proc) start end)
  (comint-send-string (*jshell*) "\n")
  (jshell-switch-to-repl t))

(defun jshell-send-last-sexp ()
  "Send the previous sexp to `*jshell*'."
  (interactive)
  (let ((bounds (region-active-if
                    (cons (region-beginning) (region-end))
                  (let ((b (jshell-last-sexp)))
                    (if (and b (< b (point)))
                        (cons b (point))
                      (bounds-of-thing-at-point 'sexp))))))
    (when bounds
      (jshell-send-region (car bounds) (cdr bounds)))))

(defun jshell-send-definition ()
  "Send the current definition to `*jshell*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (jshell-send-region (car bounds) (cdr bounds)))))

(defun jshell-send-line ()
  "Send the current line to `*jshell*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (jshell-send-region (car bounds) (cdr bounds)))))

(defun jshell-inspect-object ()
  "Inspect object."
  (interactive)
  (let ((bounds (region-active-if
                    (cons (region-beginning) (region-end))
                  (let ((b (jshell-last-symbol))
                        (s (bounds-of-thing-at-point 'symbol)))
                    (cond ((and b s) (cons (if (<= b (car s))
                                               b
                                             (car s))
                                           (if (>= (cdr s) (point))
                                               (cdr s)
                                             b)))
                          (b (cons (if (<= b (point))
                                       b
                                     (point))
                                   (point)))
                          (s (cons (if (<= (car s) (point)) (car s) (point))
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
    (define-key m "\C-x\C-e" #'jshell-send-last-sexp)
    (define-key m "\C-c\C-j" #'jshell-send-line)
    (define-key m "\C-c\C-l" #'jshell-load-file)
    (define-key m "\C-c\C-k" #'jshell-compile-file)
    (define-key m "\C-c\C-r" #'jshell-send-region)
    (define-key m "\C-c\C-z" #'jshell-switch-to-repl)
    (define-key m "\C-cI" #'jshell-inspect-object)
    m))


(make-variable-buffer-local
 (defvar jshell-mode-string nil
   "Modeline indicator for `jshell-mode'."))

(defun jshell-mode--lighter ()
  (or jshell-mode-string " Jshell"))

(defun jshell-syntax-indent ()
  "Jshell java syntax indent.")


(define-minor-mode jshell-mode
  "Toggle Jshell's mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

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



(provide 'jshell)


;; end of jshell.el
