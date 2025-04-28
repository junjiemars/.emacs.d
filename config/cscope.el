;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; features:
;;; 1. one-shot `cscope -L' `compile' command.
;;; 2. `run-cscope' REPL `comint' with `cscope -l' line-oriented.
;;; 3. `cscope-send-command', `[0,9]pattern' commands.
;;; 4. `cscope-repl-mode' redirect.
;;; 5. re-cscope.
;;; 6. cscope-find-[0,9] commands in `c-mode'.
;;;;
;; references:
;;; 1. https://cscope.sourceforge.net/
;;;;
;; use cases:
;;; 1. one-shot: M-x `cscope'.
;;; 2. interactive navigation: M-x `run-cscope'.
;;;;


;;; require

(require 'comint)
(require 'compile)

(eval-when-compile
  (require 'ed (v-home%> "config/ed"))
  (require 'marks (v-home%> "config/marks")))

(when-fn% xref-push-marker-stack xref
  (autoload 'xref-push-marker-stack "xref"))

;; end of require

;;;
;; cscope environment
;;;

(defvar *cscope-history* nil
  "History list for \\=`cscope\\='.")

(defvar *cscope-repl-history* nil
  "History list for \\=`run-cscope\\='.")

(defconst +cscope-line-regexp+
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]\\(.*\\)$"
  "Line regexp for \\=`compile\\='.")

(defun *cscope* ()
  "The REPL process buffer of \\=`*cscope*\\='."
  (get-buffer-create* "*cscope*" t))

(defun *cscope-find* (&optional no-create)
  "The output buffer of \\=`cscope-send-command\\='."
  (let ((name "*find|cscope*"))
    (if no-create
        (get-buffer name)
      (get-buffer-create* name t))))


(defun cscope--path-parse (command-line)
  (catch :br
    (dolist (x '("-P[[:blank:]]*\\([^[:blank:]]+\\)"
                 "-f[[:blank:]]*\\([^[:blank:]]+\\)"
                 "-s[[:blank:]]*\\([^:[:blank:]]+\\)"))
      (when (string-match x command-line)
        (throw :br (file-name-as-directory
                    (substring-no-properties
                     command-line (match-beginning 1) (match-end 1))))))))

;; end of cscope environment

;;;
;; `cscope-mode'
;;;

;;; `cscope-mode' env

(defvar *cscope--src-dir* nil
  "The source directory of \\=`cscope-mode\\='.")

(defun cscope--filename-parse (file)
  (cond ((inhibit-file-name-handler (file-exists-p file)) file)
        ((and (> (length *cscope--src-dir*) 0)
              (null (char-equal ?/ (aref file 0))))
         (concat *cscope--src-dir* file))
        (t file)))

(defun cscope--run-prompt ()
  (read-string-prompt
   "Run cscope (like this): " '*cscope-history*
   (format "cscope -dqL -P %s -0"
           (expand-file-name default-directory))))

;; end of `cscope-mode' env

(defun cscope-recompile ()
  "Re-cscope."
  (interactive)
  (cond ((null current-prefix-arg) (call-interactively #'recompile))
        (t (call-interactively #'cscope))))

(defun cscope-next-error-no-select (&optional n)
  (interactive "p")
  (when (eq major-mode 'cscope-mode)
    (unless (eq next-error-last-buffer (current-buffer))
      (setq next-error-last-buffer (current-buffer)))
    (next-error-no-select n)))

(defun cscope-prev-error-no-select (&optional n)
  (interactive "p")
  (cscope-next-error-no-select (- (or n 1))))

(define-compilation-mode cscope-mode "cscope-mode"
  "A minor mode of \\=`compile\\='."
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (set (make-local-variable 'compilation-directory-matcher) (list "\\`a\\`"))
  (set (make-local-variable 'compilation-warning-face) compilation-info-face)
  (set (make-local-variable 'line-number-display-limit-width) 400)
  (set (make-local-variable 'compilation-parse-errors-filename-function)
       #'cscope--filename-parse)
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
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m compilation-minor-mode-map)
    (define-key m "n" #'cscope-next-error-no-select)
    (define-key m "p" #'cscope-prev-error-no-select)
    (if (eq (*cscope-find* t) (current-buffer))
        (define-key m "g" nil)
      (define-key m "g" #'cscope-recompile))
    (use-local-map m)))

(defun cscope (&optional command-line)
  "Run cscope with user-specified COMMAND-LINE."
  (interactive (cscope--run-prompt))
  (setq *cscope--src-dir* (cscope--path-parse command-line))
  (let ((default-directory (or *cscope--src-dir* default-directory)))
    ;; or using `cd <dir> && cscope <args...>'
    (compilation-start command-line #'cscope-mode)))

;; end of `cscope-mode'

;;;
;; `cscope-repl-mode'
;;;

;;; cscope-repl env

(defvar *cscope--repl-src-dir* nil
  "The source directory of \\=`*cscope*\\='.")

(defvar *cscope--repl-recompile-history* nil
  "Re-cscope REPL history list.")

(defvar *cscope--repl-redirect* nil
  "Redirect? to \\=`*cscope-find*\\='buffer.")

(defun cscope--repl-run-prompt ()
  (read-string-prompt
   "Run cscope: " '*cscope-repl-history*
   (format "-dql -P %s " (expand-file-name default-directory))))

(defun cscope--repl-parse-filename (file)
  (cond ((file-exists-p file) file)
        ((and (> (length *cscope--repl-src-dir*) 0)
              (null (char-equal ?/ (aref file 0))))
         (concat *cscope--repl-src-dir* file))
        (t file)))

(defun cscope-repl-toggle-redirect! ()
  "Toggle the redirect of \\=`cscope-repl-mode\\' on or off."
  (interactive)
  (let ((new (null *cscope--repl-redirect*)))
    (setq *cscope--repl-redirect* new
          mode-name (if new "REPL>" "REPL"))
    (force-mode-line-update)))

(defun cscope--repl-echo-shadow (str)
  (propertize str 'font-lock-face 'shadow))

;; end of cscope-repl env

;;; cscope repl command

(defun cscope-send-command (proc command &optional echo)
  "Send COMMAND to cscope REPL PROC."
  (let ((out (*cscope-find*)))
    (with-current-buffer out
      (fluid-let (buffer-read-only nil)
        (erase-buffer)
        (insert (cscope--repl-echo-shadow (concat command "\n")))
        (comint-redirect-send-command-to-process command out proc echo nil)
        (cscope-mode))
      (define-key (current-local-map) "g" 'cscope-repl-recompile)
      (push! command *cscope--repl-recompile-history*)
      (set (make-local-variable '*cscope--src-dir*)
           *cscope--repl-src-dir*))))

(defun cscope-repl-simple-send (proc in)
  (cscope-send-command proc in t)
  (comint-snapshot-last-prompt))

(defun cscope-repl-preoutput-filter (out)
  (cond (*cscope--repl-redirect*
         (let* ((ln (strchr out ?\n))
                (in (and ln (substring-no-properties out 0 (1+ ln)))))
           (if (and in (string-match
                        "^\\(cscope:\\|Unable to search\\) .*$"
                        in 0))
               (concat (cscope--repl-echo-shadow in) ">> ")
             "")))
        (t out)))

(defun cscope-repl-send-input (&optional _ __)
  "Send input to process."
  (interactive)
  (cond (*cscope--repl-redirect*
         (let ((comint-input-sender #'cscope-repl-simple-send))
           (call-interactively 'comint-send-input))
         (pop-to-buffer (*cscope-find*)))
        (t (call-interactively 'comint-send-input))))

(defun cscope-repl-recompile ()
  "Re-cscope find."
  (interactive)
  (let ((cmd (if current-prefix-arg
                 (read-string "cscope Find: "
                              (car *cscope--repl-recompile-history*)
                              '*cscope--repl-recompmile-history*)
               (car *cscope--repl-recompile-history*))))
    (and cmd (cscope-send-command (get-buffer-process (*cscope*)) cmd))))

(defun cscope-repl-delete-output ()
  "Delete all output or last input."
  (interactive)
  (cond (*cscope--repl-redirect* (call-interactively 'comint-kill-input))
        (t (call-interactively 'comint-delete-output))))

;; cscope repl command

;;; cscope-find*

(defun cscope--find-prompt (prompt &optional default-value)
  (list (let ((s (or default-value (symbol@*))))
          (if current-prefix-arg
              (read-string prompt s)
            s))))

(defun cscope--find-command (what)
  (let* ((buf (*cscope*))
         (proc (get-buffer-process buf)))
    (with-current-buffer buf
      (unless (or proc (eq 'run (condition-case _
                                    (process-status proc)
                                  (error nil))))
        (user-error
         "%s" "No cscope living process found, try M-x run-cscope")))
    (cscope-send-command proc what)
    (when-fn% xref-push-marker-stack xref
      (xref-push-marker-stack))
    (pop-to-buffer (*cscope-find*))))

(defun cscope-find-this-c-symbol (what)
  (interactive (cscope--find-prompt "Find this C symbol: "))
  (cscope--find-command (concat "0" what)))

(defun cscope-find-this-function-definition (what)
  (interactive (cscope--find-prompt "Find this function definition: "))
  (cscope--find-command (concat "1" what)))

(defun cscope-find-functions-called-by-this-function (what)
  (interactive (cscope--find-prompt "Find functions called by this function: "))
  (cscope--find-command (concat "2" what)))

(defun cscope-find-functions-calling-this-function (what)
  (interactive (cscope--find-prompt "Find functions calling this function: "))
  (cscope--find-command (concat "3" what)))

(defun cscope-find-this-text-string (what)
  (interactive (cscope--find-prompt "Find this text string: "))
  (cscope--find-command (concat "4" what)))

(defun cscope-find-this-egrep-pattern (what)
  (interactive (cscope--find-prompt "Find this egrep pattern: "))
  (cscope--find-command (concat "6" what)))

(defun cscope-find-this-file (what)
  (interactive (cscope--find-prompt "Find this file: "))
  (cscope--find-command (concat "7" what)))

(defun cscope-find-files-including-this-file (what)
  (interactive (cscope--find-prompt "Find files #including this file: "
                                    (symbol@* 'filename)))
  (cscope--find-command (concat "8" what)))

(defun cscope-find-assignments-to-this-symbol (what)
  (interactive (cscope--find-prompt "Find assignments to this symbol: "))
  (cscope--find-command (concat "9" what)))

(defun cscope--find-define-keys (keymap)
  (define-key keymap "0" #'cscope-find-this-c-symbol)
  (define-key keymap "1" #'cscope-find-this-function-definition)
  (define-key keymap "2"
              #'cscope-find-functions-called-by-this-function)
  (define-key keymap "3"
              #'cscope-find-functions-calling-this-function)
  (define-key keymap "4" #'cscope-find-this-text-string)
  (define-key keymap "6" #'cscope-find-this-egrep-pattern)
  (define-key keymap "7" #'cscope-find-this-file)
  (define-key keymap "8" #'cscope-find-files-including-this-file)
  (define-key keymap "9" #'cscope-find-assignments-to-this-symbol))

;;  end of cscope-find*

(define-derived-mode cscope-repl-mode comint-mode "REPL"
  "Major mode for a cscope REPL process."
  (setq comint-prompt-regexp "^>> "
        comint-prompt-read-only t
        comint-use-prompt-regexp t
        mode-line-process '("" ":%s"))
  (add-hook 'comint-preoutput-filter-functions
            #'cscope-repl-preoutput-filter nil 'local)
  (let ((keymap (current-local-map)))
    (define-key keymap ">" #'cscope-repl-toggle-redirect!)
    (define-key keymap "" #'cscope-repl-send-input)
    (define-key keymap "" #'cscope-repl-delete-output))
  (with-eval-after-load 'cc-mode
    (and (boundp 'c-mode-map)
         (cscope--find-define-keys c-mode-map))
    (and (boundp 'c++-mode-map)
         (cscope--find-define-keys c++-mode-map)))
  (when-feature-treesit%
    (with-eval-after-load 'c-ts-mode
      (and (boundp 'c-ts-mode-map)
           (cscope--find-define-keys c-ts-mode-map))
      (and (boundp 'c++-ts-mode-map)
           (cscope--find-define-keys c++-ts-mode-map)))))

(defun run-cscope (&optional command-line)
  "Run a cscope REPL process, input and output via buffer *cscope*."
  (interactive (cscope--repl-run-prompt))
  (unless (comint-check-proc (*cscope*))
    (unless (executable-find* "cscope")
      (error "%s" "No cscope program found"))
    (setq *cscope--repl-src-dir* (cscope--path-parse command-line)
          *cscope--repl-redirect* nil)
    (kill-buffer (*cscope*))
    (with-current-buffer (*cscope*)
      (let ((default-directory (or *cscope--repl-src-dir*
                                   (expand-file-name default-directory))))
        (apply #'make-comint-in-buffer
               (buffer-name (current-buffer))
               (current-buffer)
               "cscope"
               nil
               (split-string* command-line "\\s-+" t)))
      (cscope-repl-mode)))
  (switch-to-buffer-other-window (*cscope*)))

;; end of `cscope-repl-mode'

(provide 'cscope)


;; end of cscope.el
