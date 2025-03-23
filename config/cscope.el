;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; cscope.el
;;;;
;; 1. `cscope -L' interact with `compile'.
;; 2. TODO: `cscope -l' interact with `comint' in `compile' mode.
;;;;



;;; require

(require 'compile)

;; end of require

;;;
;; cscope environment
;;;

(defvar *cscope-history* nil
  "Cscope history list.")

(defvar *cscope-src-dir* nil
  "Cscope source directory.")

(defconst +cscope-line-regexp+
  "^\\([^[:space:]]+\\)[[:space:]]\\([^[:space:]]+\\)[[:space:]]\\([[:digit:]]+\\)[[:space:]]\\(.*\\)$")

;; end of cscope environment

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
  ;; treat the output of cscope as `compilation warn'
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



(provide 'cscope)


;; end of cscope.el
