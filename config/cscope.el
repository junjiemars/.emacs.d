;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; features:
;;; 1. one-shot `cscope -L' `compile' command.
;;; 2. `run-cscope' REPL with `cscope -l' using `comint'.
;;; 3. `cscope-send-command', `[0,9]pattern' commands.
;;; 4. `cscope-repl-mode' redirect.
;;; 5. re-cscope.
;;;;
;; references:
;;; 1. https://cscope.sourceforge.net/
;;;;


;;; require

(require 'comint)
(require 'compile)

(eval-when-compile
  (require 'ed (v-home%> "config/ed")))

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

(defun *cscope-find* ()
  "The output buffer of \\=`cscope-send-command\\='."
  (get-buffer-create* "*find|cscope*" t))


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
    (if (eq (*cscope-find*) (current-buffer))
        (define-key m "g" nil)
      (define-key m "g" #'cscope-recompile))
    (use-local-map m)))

(defun cscope (&optional command-line)
  "Run cscope with user-specified COMMAND-LINE."
  (interactive (read-string-prompt
                "Run cscope (like this): " '*cscope-history*
                (format "cscope -dL -P %s -0"
			                  (expand-file-name default-directory))))
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

;; end of cscope-repl env

(defun cscope-send-command (proc command)
  "Send COMMAND to cscope REPL."
  (let ((out (*cscope-find*)))
    (with-current-buffer out
      (fluid-let (buffer-read-only nil)
        (erase-buffer)
        (comint-redirect-send-command-to-process command out proc t nil)
        (cscope-mode))
      (define-key (current-local-map) "g" 'cscope-repl-recompile)
      (push! command *cscope--repl-recompile-history*)
      (set (make-local-variable '*cscope--src-dir*)
           *cscope--repl-src-dir*))))

(defun cscope-repl-simple-send (proc in)
  (cscope-send-command proc in)
  (comint-snapshot-last-prompt))

(defun cscope-repl-preoutput-filter (out)
  (cond (*cscope--repl-redirect*
         (let* ((ln (strchr out ?\n))
                (in (and ln (substring-no-properties out 0 (1+ ln)))))
           (if (and in (string-match
                        "^\\(cscope: .*\\|Unable to search database\\)$"
                        in 0))
               (concat (propertize in 'font-lock-face 'shadow) ">> ")
             "")))
        (t out)))

(defun cscope-repl-send-input (&optional _ __)
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
                              '*cscope-repl--cmd-history*)
               (car *cscope--repl-recompile-history*))))
    (and cmd (cscope-send-command (get-buffer-process (*cscope*)) cmd))))

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
    (define-key keymap "" #'cscope-repl-send-input)))

(defun run-cscope (&optional command-line)
  "Run a cscope REPL process, input and output via buffer *cscope*."
  (interactive (read-string-prompt
                "Run cscope: " '*cscope-repl-history*
                (format "-dl -P %s " (expand-file-name default-directory))))
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
