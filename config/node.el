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
;; use cases:
;;; 1. M-x `node-mode', then M-x `node-load-file'.
;;; 2. M-x `run-node'.
;;;;;


;;; require

(require 'comint)

;; end of require

;;;
;; node environment
;;;

(defun nvm (command)
  "Node version manager."
  (let ((nvmsh (expand-file-name "~/.nvm/nvm.sh")))
    (if (file-exists-p nvmsh)
        (shell-command* "source" nvmsh "; nvm" command)
      (cons 1 (format "%s no found" nvmsh)))))

(defun node-program-check ()
  "Check node program path."
  (or (let ((out (nvm "which node")))
        (and (= 0 (car out)) (string-trim> (cdr out))))
      (executable-find*
       "node"
       (lambda (node)
         (let ((x (shell-command* "echo '1+2+3'|" node "-p")))
           (= 0 (car x)))))))

(defalias 'node-program
  (let ((b (node-program-check)))
    (lambda (&optional n)
      (if (null n) b (setq b n))))
  "Program invoked by the \\=`run-node\\=' command.")

(defun *node* ()
  "The current *node* process buffer."
  (get-buffer-create* "*node*" t))

(defun *node-out* ()
  "The output buffer of \\=`node-completion\\='."
  (get-buffer-create* "*out|node*" t))

(defalias '*node-start-file*
  (let ((b (v-home% ".exec/node.js")))
    (lambda ()
      (inhibit-file-name-handler
        (cond ((file-exists-p b) b)
              (t (copy-file (emacs-home% "config/node.js") b))))))
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
  (when (and spawn (null (eq 'run (car (comint-check-proc (*node*))))))
    (save-window-excursion (call-interactively #'run-node)))
  (or (get-buffer-process (*node*))
      (error "%s" "No *node* process")))

(defun node-last-symbol ()
  "Return the position of left side of the last symbol."
  (save-excursion
    (catch :br
      (while (null (or (char-equal (char-before) ?\;)
                       (char-equal (char-before) ?\n)
                       (eq (char-syntax (char-before)) ? )))
        (cond ((or (char-equal (char-before) ?.)
                   (char-equal (char-before) ?_))
               (backward-char))
              ((eq (char-syntax (char-before)) ?w)
               (backward-word))
              (t (throw :br (point))))))
    (point)))

(defvar *node-completion-filter*
  "^\\(node_emacs_apropos\\|undefined\\|[ \t]*$\\)")

(defun node-completion-read (in buffer)
  (with-current-buffer buffer
    (let ((alts nil) (xs *node-completion-filter*))
      (goto-char (point-min))
      (while (null (eobp))
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
      (with-current-buffer (current-buffer)
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
  (let ((m (make-sparse-keymap "node")))
    (set-keymap-parent m comint-mode-map)
    (define-key m "\C-c\C-b" #'node-switch-to-last-buffer)
    (use-local-map m)))

(defun run-node (&optional command-line)
  "Run a node process, input and output via buffer *node*.\n
If there is a process already running in \\=`*node*\\=', switch
to that buffer. With prefix COMMAND-LINE, allows you to edit the
command line.
Run the hook `node-repl-mode-hook' after the `comint-mode-hook'."
  (interactive (read-string-prompt "Run node: " '*node-option-history*))
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

(defun node--file-prompt (prompt)
  (comint-get-source
   prompt
   (let ((n (buffer-file-name)))
     (cons (file-name-directory n)
           (file-name-nondirectory n)))
   '(js-mode) nil))

(defun node-compile-file (file)
  "Compile a javascript FILE in \\=`*node*\\='."
  (interactive (node--file-prompt "Compile javascript file: "))
  (comint-check-source file)
  (comint-send-string (node-check-proc t)
                      (format "console.info('\"%s\"')\n" file))
  (node-switch-to-last-buffer (current-buffer))
  (node-switch-to-repl))

(defun node-load-file (file)
  "Load a javascript FILE into \\=`*node*\\='."
  (interactive (node--file-prompt "Load javascript file: "))
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

(make-variable-buffer-local
 (defvar node-mode-string nil
   "Modeline indicator for \\=`node-mode\\='."))

(defun node-syntax-indent ()
  "Node javascript syntax indent.")

(defun node--mode-keymap ()
  (let ((m (make-sparse-keymap)))
    (define-key m "\M-\C-x" #'node-send-definition)
    (define-key m "" #'node-send-line)
    (define-key m "" #'node-load-file)
    (define-key m "" #'node-compile-file)
    (define-key m "" #'node-send-region)
    (define-key m "" #'node-switch-to-repl)
    (define-key m "" #'node-inspect-object)
    m))

(defvar node-mode-map (node--mode-keymap)
  "The keymap of \\=`node-mode\\='.")

(define-minor-mode node-mode
  "Toggle Node's mode.\n
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.\n
When Node mode is enabled, a host of nice utilities for
interacting with the Node REPL is at your disposal.
\\{node-mode-map}"
  :init-value nil
  :lighter " Node"
  :keymap node-mode-map
  :group 'node
  (add-hook (if-var% completion-at-point-functions minibuffer
                     'completion-at-point-functions
              'comint-dynamic-complete-functions)
            #'node-completion 0 'local)
  (node-syntax-indent))

;; end of `node-mode'


(provide 'node)


;; end of node.el
