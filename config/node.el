;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; node.el
;;;;
;;; https://nodejs.org
;;; https://github.com/abicky/nodejs-repl.el/blob/develop/nodejs-repl.el
;;;
;;; fetures:
;;; 1. start parameterized node process.
;;; 2. evaluate region, last-sexp, definition, current line.
;;; 3. load javascript file.
;;;
;;; bugs:
;;; 
;;;;;

(require 'comint)
;; (require 'thingatpt)



;; variable declarations

(defgroup node nil
  "Run a node process in a buffer."
  :group 'node)


(defun nvm (command)
  "Node version manager."
  (let ((nvmsh (expand-file-name "~/.nvm/nvm.sh")))
    (if (file-exists-p nvmsh)
        (shell-command* "source" nvmsh "; nvm" command)
      (cons 1 (format "%s no found" nvmsh)))))


(defalias 'node-program
  (lexical-let% ((b (let ((nvmout (nvm "which node")))
                      (cond ((zerop (car nvmout))
                             (string-trim> (cdr nvmout)))
                            ((executable-find%
                              "node"
                              (lambda (node)
                                (let ((x (shell-command* "echo"
                                           "'1+2+3'|" node "-")))
                                  (zerop (car x)))))
                             "node")
                            (t "node")))))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program invoked by the `run-node' command.")


(defalias '*node*
  (lexical-let% ((b))
    (lambda (&optional n)
      (cond ((not (null n))
             (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*node*")))
            (t b))))
  "The current *node* process buffer.")


(defconst +node-emacs-library+
  "console.log('start file');
" 
  "The library of node-emacs.")


(defalias '*node-start-file*
  (lexical-let% ((b (v-home% ".exec/.node.ss")))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (unless (file-exists-p b)
          (save-str-to-file +node-emacs-library+ b))
        b)))
  "The `*node*' process start file.")


(defalias 'node-switch-to-last-buffer
  (lexical-let% ((b))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (when b (switch-to-buffer-other-window b)))))
  "Switch to the last `node-mode' buffer from `*node*' buffer.")


(defvar *node-option-history* nil
  "Node option history list.")


 ;; end variable declarations


(defun node-check-proc (&optional spawn)
  "Return the `*node*' process or start one if necessary."
  (when (and spawn
             (not (eq 'run (car (comint-check-proc (*node*))))))
    (save-window-excursion (call-interactively #'run-node)))
  (or (get-buffer-process (*node*))
      (error "No `*node*' process.")))


(define-derived-mode node-repl-mode comint-mode "REPL"
  "Major mode for interacting with a node process.

The following commands are available:
\\{node-repl-mode-map}

A node process can be fired up with M-x `run-node'.

Customization: 
Entry to this mode runs the hooks on `comint-mode-hook' and
  `node-repl-mode-hook' (in that order)."
  :group 'node                          ; keyword args
  (setq comint-prompt-regexp "^[^>\n-\"]*>+ *")
  (setq comint-prompt-read-only t)
  (comment (setq comint-input-filter #'node-input-filter))
  (comment (setq comint-get-old-input #'node-get-old-input))
  (comment
   (add-hook 'comint-preoutput-filter-functions
             #'node-preoutput-filter nil 'local))
  (comment (scheme-mode-variables))
  (use-local-map node-repl-mode-map)
  (comment (node-syntax-table))
  (setq mode-line-process '("" ":%s")))


(defun run-node (&optional command-line)
  "Run a node process, input and output via buffer *node*.

If there is a process already running in `*node*', switch to that
buffer. With prefix COMMAND-LINE, allows you to edit the command
line.

Run the hook `node-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (list (read-string "Run node: "
                                  (car *node-option-history*)
                                  '*node-option-history*)))
  (unless (comint-check-proc (*node*))
    (with-current-buffer (*node*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             (node-program)
             nil                        ; no start file
             (split-string* command-line "\\s-+" t))
      (node-repl-mode)
      (comment
       (add-hook (if-var% completion-at-point-functions 'minibuffer
                          'completion-at-point-functions
                   'comint-dynamic-complete-functions)
                 #'node-completion 0 'local))))
  (switch-to-buffer-other-window (*node*)))


(defun node-switch-to-repl (&optional no-select)
  "Switch to the `*node*' buffer.

If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (node-check-proc t)
  (node-switch-to-last-buffer (current-buffer))
  (if no-select
      (display-buffer (*node*)
                      (if-fn% 'display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*node*))
    (push-mark)
    (goto-char (point-max))))


 ;; end of REPL


(defun node-compile-file (file)
  "Compile a javascript FILE in `*node*'."
  (interactive (comint-get-source
                "Compile javascript file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(scheme-mode) nil))
  (comint-check-source file)
  (comint-send-string (node-check-proc t)
                      (format "console.info('\"%s\"')\n" file))
  (node-switch-to-last-buffer (current-buffer))
  (node-switch-to-repl))

(defun node-load-file (file)
  "Load a javascript FILE into `*node*'."
  (interactive (comint-get-source
                "Load javascript file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(js-mode) nil))
  (comint-check-source file) 
  (comint-send-string (node-check-proc t)
                      (format "process.chdir('%s');\n.load %s\n"
                              (file-name-directory file)
                              (file-name-nondirectory file)))
  (node-switch-to-last-buffer (current-buffer))
  (node-switch-to-repl))


(defun node-send-region (start end)
  "Send the current region to `*node*'."
  (interactive "r")
  (process-send-region (node-check-proc) start end)
  (comint-send-string (*node*) "\n")
  (node-switch-to-repl t))

(defun node-send-last-sexp ()
  "Send the previous sexp to `*node*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (node-send-region (car bounds) (cdr bounds)))))

(defun node-send-definition ()
  "Send the current definition to `*node*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (node-send-region (car bounds) (cdr bounds)))))

(defun node-send-line ()
  "Send the current line to `*node*'."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (node-send-region (car bounds) (cdr bounds)))))


(defvar node-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'node-send-definition)
    (define-key m "\C-x\C-e" #'node-send-last-sexp)
    (define-key m "\C-x\C-j" #'node-send-line)
    (define-key m "\C-c\C-l" #'node-load-file)
    (define-key m "\C-c\C-k" #'node-compile-file)
    (define-key m "\C-c\C-r" #'node-send-region)
    (define-key m "\C-c\C-z" #'node-switch-to-repl)
    m))


(make-variable-buffer-local
 (defvar node-mode-string nil
   "Modeline indicator for `node-mode'."))

(defun node-mode--lighter ()
  (or node-mode-string " Node"))

(defun node-syntax-indent ()
  "Node javascript syntax indent.")


(define-minor-mode node-mode
  "Toggle Node's mode.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When Node mode is enabled, a host of nice utilities for
interacting with the Node REPL is at your disposal.
\\{node-mode-map}"
  :init-value nil
  :lighter (:eval (node-mode--lighter))
  :group 'node-mode
  :keymap node-mode-map
  (comment (add-hook (if-var% completion-at-point-functions 'minibuffer
                              'completion-at-point-functions
                       'comint-dynamic-complete-functions)
                     #'node-completion 0 'local))
  (node-syntax-indent))



(provide 'node)


;; end of node.el
