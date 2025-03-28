;;;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; node.el
;;;;
;; fetures:
;;; 1. start parameterized node process.
;;; 2. nvm supports.
;;; 3. switch/back to node REPL.
;;; 4. send sexp/definition/region to node REPL.
;;; 5. completion in REPL and javascript source.
;;;;
;; references:
;;; https://nodejs.org
;;;;
;;; bugs:
;;;
;;;;;

;;; require

(require 'comint)

;; end of require

;;;
;; node environment
;;;

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
  (let ((b (or (let ((out (nvm "which node")))
                 (and (zerop (car out))
                      (string-trim> (cdr out))))
               (executable-find%
                "node"
                (lambda (node)
                  (let ((x (shell-command* "echo"
                             "'1+2+3'|" node "-p")))
                    (zerop (car x)))))
               "node")))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program invoked by the \\=`run-node\\=' command.")

(defalias '*node*
  (let ((b))
    (lambda (&optional n)
      (cond (n (setq b (get-buffer-create n)))
            ((or (null b) (not (buffer-live-p b)))
             (setq b (get-buffer-create "*node*")))
            (t b))))
  "The current *node* process buffer.")

(defalias '*node-out*
  (let ((b "*out|node*"))
    (lambda (&optional n)
      (if n (setq b n)
        (get-buffer-create b))))
  "The output buffer of \\=`node-completion\\='.")

(defalias '*node-start-file*
  (let ((b (v-home% ".exec/node.js")))
    (lambda ()
      (cond ((file-exists-p b) b)
            (t (copy-file (emacs-home% "config/node.js") b)))))
  "the \\=`*node*\\=' process start file.")

(defalias 'node-switch-to-last-buffer
  (let ((b))
    (lambda (&optional n)
      (interactive)
      (if n (setq b n)
        (when b (switch-to-buffer-other-window b)))))
  "Switch to the last \\=`node-mode\\=' buffer from \\=`*node*\\=' buffer.")

(defvar *node-option-history* nil
  "Node option history list.")

;; end node environment

;;;
;; proc
;;;

(defun node-check-proc (&optional spawn)
  "Return the \\=`*node*\\=' process or start one if necessary."
  (when (and spawn (not (eq 'run (car (comint-check-proc (*node*))))))
    (save-window-excursion (call-interactively #'run-node)))
  (or (get-buffer-process (*node*))
      (error "%s" "No *node* process")))

(defun node-last-symbol ()
  "Return the position of left side of the last symbol."
  (save-excursion
    (catch 'br
      (while (not (or (char-equal (char-before) ?\;)
                      (char-equal (char-before) ?\n)
                      (eq (char-syntax (char-before)) ? )))
        (cond ((or (char-equal (char-before) ?.)
                   (char-equal (char-before) ?_))
               (backward-char))
              ((eq (char-syntax (char-before)) ?w)
               (backward-word))
              (t (throw 'br (point))))))
    (point)))

(defvar *node-completion-filter*
  "^\\(node_emacs_apropos\\|undefined\\|[ \t]*$\\)")

(defun node-completion-read (in buffer)
  (with-current-buffer buffer
    (let ((alts nil) (xs *node-completion-filter*))
      (goto-char (point-min))
      (while (not (eobp))
        (let ((ln (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
          (cond ((string-match xs ln) nil)
                ((string-equal in ln) nil)
                (t (setq alts (nconc alts (list ln))))))
        (forward-line 1))
      alts)))

(defun node-completion ()
  (interactive)
  (let* ((node (*node*)) (proc (get-buffer-process node))
         (start nil) (end nil) (in nil))
    (when proc
      (with-current-buffer node
        (let ((bs (cons (save-excursion (node-last-symbol)) (point))))
          (setq start (car bs)
                end (cdr bs)
                in (buffer-substring-no-properties start end))))
      (let ((cmd (format "node_emacs_apropos(\"%s\",128)" in))
            (out (*node-out*)))
        (with-current-buffer out (erase-buffer))
        (comint-redirect-send-command-to-process cmd out proc nil t)
        (with-current-buffer node
          (unwind-protect
              (while (or quit-flag (null comint-redirect-completed))
                (accept-process-output proc 2))
            (comint-redirect-cleanup)))
        (list start end (node-completion-read in out)
              :exclusive 'no)))))

;; end of proc

;;;
;; REPL
;;;

(defvar node-repl-mode-map
  (let ((m (make-sparse-keymap "node")))
    (define-key m "\C-c\C-b" #'node-switch-to-last-buffer)
    m)
  "The keymap for `*node*' REPL.")

(define-derived-mode node-repl-mode comint-mode "REPL"
  "Major mode for interacting with a node process.\n
The following commands are available:
\\{node-repl-mode-map}\n
A node process can be fired up with M-x `run-node'.
Customization:
Entry to this mode runs the hooks on `comint-mode-hook' and
  \\=`node-repl-mode-hook\\=' (in that order)."
  :group 'node                          ; keyword args
  (setq comint-prompt-regexp "^[^>\n-\"]*>+ *"
        comint-prompt-read-only t
        mode-line-process '("" ":%s"))
  (use-local-map node-repl-mode-map))

(defun run-node (&optional command-line)
  "Run a node process, input and output via buffer *node*.\n
If there is a process already running in \\=`*node*\\=', switch
to that buffer. With prefix COMMAND-LINE, allows you to edit the
command line.
Run the hook `node-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (list (read-string "Run node: "
                                  (car *node-option-history*)
                                  '*node-option-history*)))
  (unless (comint-check-proc (*node*))
    (unless (node-program)
      (error "%s" "No node program found"))
    (with-current-buffer (*node*)
      (apply #'make-comint-in-buffer
             (buffer-name (current-buffer))
             (current-buffer)
             (node-program)
             (*node-start-file*)
             (split-string* command-line "\\s-+" t))
      (node-repl-mode)
      (add-hook (if-var% completion-at-point-functions minibuffer
                         'completion-at-point-functions
                  'comint-dynamic-complete-functions)
                #'node-completion 0 'local)
      (setq comint-process-echoes t)))
  (switch-to-buffer-other-window (*node*)))


(defun node-switch-to-repl (&optional no-select)
  "Switch to the \\=`*node*\\=' buffer.\n
If NO-SELECT is nil then select the buffer and put the cursor at
end of buffer, otherwise just popup the buffer."
  (interactive "P")
  (node-check-proc t)
  (node-switch-to-last-buffer (current-buffer))
  (if no-select
      (display-buffer (*node*)
                      (if-fn% display-buffer-pop-up-window nil
                              #'display-buffer-pop-up-window
                        t))
    ;; switch to REPL and select it
    (pop-to-buffer (*node*))
    (push-mark)
    (goto-char (point-max))))

 ;; end of REPL

;;;
;; compile / load
;;;

(defun node-compile-file (file)
  "Compile a javascript FILE in \\=`*node*\\='."
  (interactive (comint-get-source
                "Compile javascript file: "
                (let ((n (buffer-file-name)))
                  (cons (file-name-directory n)
                        (file-name-nondirectory n)))
                '(js-mode) nil))
  (comint-check-source file)
  (comint-send-string (node-check-proc t)
                      (format "console.info('\"%s\"')\n" file))
  (node-switch-to-last-buffer (current-buffer))
  (node-switch-to-repl))

(defun node-load-file (file)
  "Load a javascript FILE into \\=`*node*\\='."
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

;; end of compile / load

;;;
;; `node-mode'
;;;

(defun node-send-region (start end)
  "Send the current region to \\=`*node*\\='."
  (interactive "r")
  (process-send-region (node-check-proc) start end)
  (comint-send-string (*node*) "\n")
  (node-switch-to-repl t))

(defun node-send-definition ()
  "Send the current definition to \\=`*node*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'defun)))
    (when bounds
      (node-send-region (car bounds) (cdr bounds)))))

(defun node-send-line ()
  "Send the current line to \\=`*node*\\='."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'line)))
    (when bounds
      (node-send-region (car bounds) (cdr bounds)))))

(defun node-inspect-object ()
  "Inspect object."
  (interactive)
  (let ((bounds (if-region-active
                    (cons (region-beginning) (region-end))
                  (let ((b (node-last-symbol))
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
        (node-send-region lhs rhs)))))


(defvar node-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'node-send-definition)
    (define-key m "\C-c\C-j" #'node-send-line)
    (define-key m "\C-c\C-l" #'node-load-file)
    (define-key m "\C-c\C-k" #'node-compile-file)
    (define-key m "\C-c\C-r" #'node-send-region)
    (define-key m "\C-c\C-z" #'node-switch-to-repl)
    (define-key m "\C-cI" #'node-inspect-object)
    m))


(make-variable-buffer-local
 (defvar node-mode-string nil
   "Modeline indicator for \\=`node-mode\\='."))

(defun node-mode--lighter ()
  (or node-mode-string " Node"))

(defun node-syntax-indent ()
  "Node javascript syntax indent.")


(define-minor-mode node-mode
  "Toggle Node's mode.\n
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.\n
When Node mode is enabled, a host of nice utilities for
interacting with the Node REPL is at your disposal.
\\{node-mode-map}"
  :init-value nil
  :lighter (:eval (node-mode--lighter))
  :group 'node-mode
  :keymap node-mode-map
  (add-hook (if-var% completion-at-point-functions minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'node-completion 0 'local)
  (node-syntax-indent))

;; end of `node-mode'


(provide 'node)


;; end of node.el
