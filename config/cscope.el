;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; 1. `cscope -L' interact with `compile'.
;; 2. `cscope -l' interact with `comint'.
;; 3. `cscope-repl-mode' interact with `xref'.
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

(defvar *cscope-src-dir* nil
  "Cscope source directory.")

(defconst +cscope-line-regexp+
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]\\(.*\\)$"
  "Line regexp for \\=`compile\\='.")

(defalias '*cscope*
  (let ((b))
    (lambda (&optional n)
      (cond (n (setq b (get-buffer-create* n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create* "*cscope*")))
            (t b))))
  "The *cscope* REPL process buffer.")

(defalias '*cscope-out*
  (let ((b "*out|cscope*"))
    (lambda (&optional n)
      (cond (n (setq b n))
            (t (get-buffer-create* b)))))
  "The output buffer of \\=`cscope-send-command'\\=.")

;; end of cscope environment

;;;
;; `cscope-mode'
;;;

(defun cscope--parse-filename (file)
  (cond ((file-exists-p file) file)
        ((null (char-equal ?/ (aref file 0))) (concat *cscope-src-dir* file))
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
  (set (make-local-variable 'compilation-mode-line-errors)
       `(" ["
         (:propertize (:eval (number-to-string compilation-num-errors-found))
							        face compilation-error
                      help-echo "Number of errors so far")
         " "
         (:propertize (:eval (number-to-string compilation-num-infos-found))
							        face compilation-warning
                      help-echo "Number of warnings so far")
         " "
         (:propertize (:eval (number-to-string compilation-num-warnings-found))
                      face compilation-info
                      help-echo "Number of informational messages so far")
         "]"))
  (let ((cscope-keymap (make-sparse-keymap)))
    (set-keymap-parent cscope-keymap compilation-minor-mode-map)
    (define-key cscope-keymap (kbd% "g") #'cscope-recompile)
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
  (setq *cscope-src-dir*
        (or (if (string-match
                 "-f[[:blank:]]*\\([^[:blank:]]+\\)" command-line)
                (file-name-directory
                 (substring-no-properties
                  command-line (match-beginning 1) (match-end 1)))
              default-directory)))
  (compilation-start command-line #'cscope-mode))

;; end of `cscope-mode'

;;;
;; `cscope-repl-mode'
;;;

(defun cscope-send-command (command)
  "Send COMMAND to cscope REPL."
  (let ((out (*cscope-out*)))
    (with-current-buffer out (erase-buffer))
    (comint-redirect-send-command-to-process command out (*cscope*) nil t)))

(define-derived-mode cscope-repl-mode comint-mode "REPL"
  "Major mode for a cscope REPL process."
  (setq comint-prompt-regexp "^>>"
        comint-prompt-read-only t)
  (setq mode-line-process '("" ":%s")))

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
  (switch-to-buffer-other-window (*cscope*)))

;; end of `cscope-repl-mode'


(provide 'cscope)


;; end of cscope.el
