;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; 1. `compile' interact with cscope -L .
;; 2. todo: `comint' interact with cscope -l and in `compile' mode.
;;;;



;;; require

(require 'compile)

;; end of require

;;;
;; cscope environment
;;;

(defgroup cscope nil
  "Run a cscope process in a buffer."
  :group 'tools
  :group 'cscope)

(defvar *cscope-option-history*
  (list "cscope -dL -0 ")
  "cscope option history list.")

(defvar *cscope-last-buffer* nil
  "The most recent cscope buffer.")

(defvar *cscope-src-dir* nil)

(defconst +cscope-line-regexp+
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]\\(.*\\)$")

(defconst +cscope-error-regexp+
  `((,+cscope-line-regexp+ 1 3 nil (1 . 0) 1)))

;; end of cscope environment

(defun cscope-parse-errors-filename (file)
  (cond ((file-exists-p file) file)
        (t (concat *cscope-src-dir* file))))

(define-compilation-mode cscope-mode "cscope-mode"
  (setq cscope-last-buffer (current-buffer))
  (set (make-local-variable 'compilation-disable-input) t)
  (set (make-local-variable 'compilation-error-screen-columns) nil)
  (set (make-local-variable 'compilation-warning-face)
       compilation-info-face)
  (set (make-local-variable 'compilation-parse-errors-filename-function)
       #'cscope-parse-errors-filename)
  (set (make-local-variable 'compilation-error-regexp-alist)
       +cscope-error-regexp+)
  (set (make-local-variable 'compilation-directory-matcher)
       (list "\\`a\\`")))

(defun cscope (&optional command-line)
  "Run cscope with user-specified COMMAND-ARGS."
  (interactive (list (funcall (if-fn% read-shell-command nil
                                      #'read-shell-command
                                #'read-string)
                              "Run cscope (like this): "
                              (car *cscope-option-history*)
                              '*cscope-option-history*)))
  (setq *cscope-src-dir*
        (or (if (string-match
                 "-f *\\([^[:blank:]]+\\)" command-line)
                (file-name-directory
                 (substring-no-properties
                  command-line (match-beginning 1) (match-end 1)))
              default-directory)))
  (compilation-start command-line #'cscope-mode))



(provide 'cscope)


;; end of cscope.el
