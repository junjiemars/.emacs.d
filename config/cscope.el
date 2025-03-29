;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; 1. `cscope -L' interact with `compile'.
;; 2. `cscope -l' interact with `comint'.
;; 3. `cscope-find-c-symbol' [0,9] commands.
;;;;



;;; require

(require 'comint)
(require 'compile)

;; end of require

;;;
;; cscope environment
;;;

(defvar *cscope-history* nil
  "History list for \\=`cscope\\='.")

(defvar *cscope-repl-history* nil
  "History list for \\=`run-cscope\\='.")

(defconst +cscope-line-regexp+
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]+\\(.*\\)$"
  "Line regexp for \\=`compile\\='.")

(defun *cscope* ()
  "The *cscope* REPL process buffer."
  (get-buffer-create* "*cscope*"))

(defun *cscope-out* ()
  "The output buffer of \\=`cscope-send-command'\\=."
  (get-buffer-create* "*out|cscope*"))

(defun cscope--parse-src-dir (command-line)
  (or (if (string-match
           "-f[[:blank:]]*\\([^[:blank:]]+\\)" command-line)
          (file-name-directory
           (substring-no-properties
            command-line (match-beginning 1) (match-end 1)))
        default-directory)))

;; end of cscope environment

;;;
;; `cscope-mode'
;;;

(defvar *cscope--src-dir* nil
  "Cscope source directory.")

(defun cscope--parse-filename (file)
  (cond ((file-exists-p file) file)
        ((and (> (length *cscope--src-dir*) 0)
              (null (char-equal ?/ (aref file 0))))
         (concat *cscope--src-dir* file))
        (t file)))

(defun cscope-recompile ()
  "Re-cscope."
  (interactive)
  (cond ((null current-prefix-arg) (call-interactively #'recompile))
        (t (call-interactively #'cscope))))

(define-compilation-mode cscope-mode "cscope-mode"
  "A minor mode of \\=`compile\\='."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (set (make-local-variable 'compilation-directory-matcher) (list "\\`a\\`"))
  (set (make-local-variable 'compilation-warning-face) compilation-info-face)
  (set (make-local-variable 'compilation-parse-errors-filename-function)
       #'cscope--parse-filename)
  ;; treat the output of cscope as `compilation warnings'
  (set (make-local-variable 'compilation-error-regexp-alist)
       `((,+cscope-line-regexp+ 1 3 nil 1 1)))
  ;; swap `compilation-num-warnings-found' and `compilation-num-infos-found'
  (when-var% compilation-mode-line-errors compile
    (set (make-local-variable 'compilation-mode-line-errors)
         `(" [" (:propertize
                 (:eval (number-to-string compilation-num-errors-found))
						     face compilation-error
                 help-echo "Number of errors so far")
           " " (:propertize
                (:eval (number-to-string compilation-num-infos-found))
						    face compilation-warning
                help-echo "Number of warnings so far")
           " " (:propertize
                (:eval (number-to-string compilation-num-warnings-found))
                face compilation-info
                help-echo "Number of informational messages so far")
           "]")))
  (let ((cscope-keymap (make-sparse-keymap)))
    (set-keymap-parent cscope-keymap compilation-minor-mode-map)
    (define-key cscope-keymap "g" #'cscope-recompile)
    (if-key% compilation-minor-mode-map "n" #'next-error-no-select
             (comment "ignore if Emacs25+")
      (define-key cscope-keymap "n" #'next-error-no-select)
      (define-key cscope-keymap "p" #'previous-error-no-select))
    (use-local-map cscope-keymap)))

(defun cscope (&optional command-line)
  "Run cscope with user-specified COMMAND-LINE."
  (interactive (list (funcall (if-fn% read-shell-command nil
                                      #'read-shell-command
                                #'read-string)
                              "Run cscope (like this): "
                              (or (car *cscope-history*)
                                  (format
                                   "cscope -dL -P %s -f %scscope.out -0 "
                                   default-directory default-directory))
                              '*cscope-history*)))
  (setq *cscope--src-dir* (cscope--parse-src-dir command-line))
  (compilation-start command-line #'cscope-mode))

;; end of `cscope-mode'

;;;
;; `cscope-repl-mode'
;;;

(defvar *cscope--repl-src-dir* nil
  "Cscope REPL source directory.")

(defun cscope--repl-parse-filename (file)
  (cond ((file-exists-p file) file)
        ((and (> (length *cscope--repl-src-dir*) 0)
              (null (char-equal ?/ (aref file 0))))
         (concat *cscope--repl-src-dir* file))
        (t file)))

(defun cscope-send-command (command &optional no-display in-cscope-mode)
  "Send COMMAND to cscope REPL."
  (unless (eq 'run (car (comint-check-proc (*cscope*))))
    (user-error "%s" "No *cscope* process"))
  (let ((out (*cscope-out*))
        (proc (get-buffer-process (*cscope*))))
    (with-current-buffer out
      (fluid-let (buffer-read-only nil)
        (erase-buffer)
        (comint-redirect-send-command-to-process
         command out proc nil no-display)
        (with-current-buffer (*cscope*)
          (unwind-protect
              (while (or quit-flag (null comint-redirect-completed))
                (accept-process-output proc 2))
            (comint-redirect-cleanup)))
        (when in-cscope-mode
          (cscope-mode)
          (set (make-local-variable '*cscope--src-dir*)
               *cscope--repl-src-dir*))))))

(defun cscope--find-symbol (symbol &optional no-display in-cscope-mode)
  (cscope-send-command (concat "0" symbol) no-display in-cscope-mode))

(defun cscope--find-definition (symbol &optional no-display in-cscope-mode)
  (cscope-send-command (concat "1" symbol) no-display in-cscope-mode))

(defun cscope--find-prompt (prompt)
  (list (read-string prompt)))

(defun cscope-find-c-symbol (&optional symbol)
  (interactive (cscope--find-prompt "Find this C symbol: "))
  (cscope--find-symbol symbol nil t))

(defun cscope-find-this-function-definition (&optional symbol)
  (interactive (cscope--find-prompt "Find this function definition: "))
  (cscope--find-definition symbol nil t))

(defun cscope-find-functions-called-by-this-function (&optional symbol)
  (interactive
   (cscope--find-prompt "Find functions called by this function: "))
  (cscope-send-command (concat "2" symbol) nil t))

(defun cscope-find-functions-calling-by-function (&optional symbol)
  (interactive (cscope--find-prompt "Find functions calling this function: "))
  (cscope-send-command (concat "3" symbol) nil t))

(defun cscope-find-this-text-string (&optional text)
  (interactive (cscope--find-prompt "Find this text string: "))
  (cscope-send-command (concat "4" text) nil t))

(defun cscope-find-this-egrep-pattern (&optional pattern)
  (interactive (cscope--find-prompt "Find this egrep pattern: "))
  (cscope-send-command (concat "6" pattern) nil t))

(defun cscope-find-this-file (&optional filename)
  (interactive (cscope--find-prompt "Find this file: "))
  (cscope-send-command (concat "7" filename) nil t))

(defun cscope-find-files-including-this-file (&optional symbol)
  (interactive (cscope--find-prompt "Find files #including this file: "))
  (cscope-send-command (concat "8" symbol) nil t))

(defun cscope-find-assignments-to-this-symbol (&optional symbol)
  (interactive (cscope--find-prompt "Find assignments to this symbol: "))
  (cscope-send-command (concat "9" symbol) nil t))

(defun cscope--line-count ()
  (let ((n nil))
    (with-current-buffer (*cscope-out*)
      (goto-char (point-min))
      (setq n (string-match*
               "cscope: \\([0-9]+\\) lines"
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               1))
      (forward-line 1))
    (and n (string-to-number n))))

(defun cscope--line-fields (line)
  (and
   (string-match +cscope-line-regexp+ line)
   (list
    :file (substring-no-properties line (match-beginning 1) (match-end 1))
    :scope (substring-no-properties line (match-beginning 2) (match-end 3))
    :line (substring-no-properties line (match-beginning 3) (match-end 3))
    :text (substring-no-properties line (match-beginning 4) (match-end 4)))))

(define-derived-mode cscope-repl-mode comint-mode "REPL"
  "Major mode for a cscope REPL process."
  (setq comint-prompt-regexp "^>>"
        comint-prompt-read-only t
        mode-line-process '("" ":%s")))

(defun run-cscope (&optional command-line)
  "Run a cscope REPL process, input and output via buffer *cscope*."
  (interactive (list (funcall (if-fn% read-shell-command nil
                                      #'read-shell-command
                                #'read-string)
                              "Run cscope: "
                              (or (car *cscope-repl-history*)
                                  "-dl -f ")
                              '*cscope-repl-history*)))
  (unless (comint-check-proc (*cscope*))
    (unless (executable-find* "cscope")
      (error "%s" "No cscope program found"))
    (with-current-buffer (*cscope*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             "cscope"
             nil
             (split-string* command-line "\\s-+" t))
      (cscope-repl-mode)))
  (setq *cscope--repl-src-dir* (cscope--parse-src-dir command-line))
  (switch-to-buffer-other-window (*cscope*)))

;; end of `cscope-repl-mode'

(provide 'cscope)


;; end of cscope.el
