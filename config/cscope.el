;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; features:
;;; 1. one-shot `cscope -L' `compile' command.
;;; 2. `run-cscope' interacts with `cscope -l' using `comint'.
;;; 3. `cscope-find-this-c-symbol', `-[0,9]pattern' commands.
;;;;
;; references:
;;; 1. https://cscope.sourceforge.net/
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
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]\\(.*\\)$"
  "Line regexp for \\=`compile\\='.")

(defun *cscope* ()
  "The REPL process buffer of \\=`*cscope*\\='."
  (get-buffer-create* "*cscope*"))

(defun *cscope-out* ()
  "The output buffer of \\=`cscope-send-command\\='."
  (get-buffer-create* "*out|cscope*"))

(defun cscope--path-parse (command-line)
  (cond ((string-match "-P[[:blank:]]*\\([^[:blank:]]+\\)" command-line)
         (file-name-as-directory
          (substring-no-properties
           command-line (match-beginning 1) (match-end 1))))
        ((string-match "-f[[:blank:]]*\\([^[:blank:]]+\\)" command-line)
         (file-name-directory
          (substring-no-properties
           command-line (match-beginning 1) (match-end 1))))
        (t default-directory)))

;; end of cscope environment

;;;
;; `cscope-mode'
;;;

(defvar *cscope--src-dir* nil
  "The source directory of \\=`cscope-mode\\='.")

(defun cscope--filename-parse (file)
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
    (if (eq (*cscope-out*) (current-buffer))
        (define-key m "g" nil)
      (define-key m "g" #'cscope-recompile))
    (use-local-map m)))

(defun cscope (&optional command-line)
  "Run cscope with user-specified COMMAND-LINE."
  (interactive (read-string-prompt
                "Run cscope (like this): "
                '*cscope-history*
                (format
                 "cscope -dL -P %s -f %scscope.out -0 "
                 default-directory default-directory)))
  (setq *cscope--src-dir* (cscope--path-parse command-line))
  (compilation-start command-line #'cscope-mode))

;; end of `cscope-mode'

;;;
;; `cscope-repl-mode'
;;;

(defvar *cscope--repl-src-dir* nil
  "The source directory of \\=`*cscope*\\='.")

(defun cscope--repl-parse-filename (file)
  (cond ((file-exists-p file) file)
        ((and (> (length *cscope--repl-src-dir*) 0)
              (null (char-equal ?/ (aref file 0))))
         (concat *cscope--repl-src-dir* file))
        (t file)))

(defun cscope-send-command (command)
  "Send COMMAND to cscope REPL."
  (unless (eq 'run (car (comint-check-proc (*cscope*))))
    (user-error "%s" "No *cscope* process"))
  (let ((out (*cscope-out*))
        (proc (get-buffer-process (*cscope*))))
    (with-current-buffer out
      (fluid-let (buffer-read-only nil)
        (erase-buffer)
        (comint-redirect-send-command-to-process command out proc nil)
        (cscope-mode)
        (set (make-local-variable '*cscope--src-dir*) *cscope--repl-src-dir*)))
    (switch-to-buffer-other-window out)))

(defun cscope-find-this-c-symbol (&optional symbol)
  (interactive "sFind this C symbol: ")
  (cscope-send-command (concat "0" symbol)))

(defun cscope-find-this-function-definition (&optional symbol)
  (interactive "sFind this function definition: ")
  (cscope-send-command (concat "1" symbol)))

(defun cscope-find-functions-called-by-this-function (&optional symbol)
  (interactive "sFind functions called by this function: ")
  (cscope-send-command (concat "2" symbol)))

(defun cscope-find-functions-calling-this-function (&optional symbol)
  (interactive "sFind functions calling this function: ")
  (cscope-send-command (concat "3" symbol)))

(defun cscope-find-this-text-string (&optional text)
  (interactive "sFind this text string: ")
  (cscope-send-command (concat "4" text)))

(defun cscope-find-this-egrep-pattern (&optional pattern)
  (interactive "sFind this egrep pattern: ")
  (cscope-send-command (concat "6" pattern)))

(defun cscope-find-this-file (&optional filename)
  (interactive "sFind this file: ")
  (cscope-send-command (concat "7" filename)))

(defun cscope-find-files-including-this-file (&optional symbol)
  (interactive "sFind files #including this file: ")
  (cscope-send-command (concat "8" symbol)))

(defun cscope-find-assignments-to-this-symbol (&optional symbol)
  (interactive "sFind assignments to this symbol: ")
  (cscope-send-command (concat "9" symbol)))

(define-derived-mode cscope-repl-mode comint-mode "REPL"
  "Major mode for a cscope REPL process."
  (setq comint-prompt-regexp "^>>"
        comint-prompt-read-only t
        mode-line-process '("" ":%s")))

(defun run-cscope (&optional command-line)
  "Run a cscope REPL process, input and output via buffer *cscope*."
  (interactive (read-string-prompt
                "Run cscope: " '*cscope-repl-history* "-dl -f "))
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
  (setq *cscope--repl-src-dir* (cscope--path-parse command-line))
  (switch-to-buffer-other-window (*cscope*)))

;; end of `cscope-repl-mode'

(provide 'cscope)


;; end of cscope.el
