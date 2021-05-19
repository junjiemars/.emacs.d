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
      (cond ((not (null n)) (setq b n))
            (t b))))
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


(defvar *node-option-history* nil
  "Node option history list.")


 ;; end variable declarations


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
      (comment (node-repl-mode))
      (comment
       (add-hook (if-var% completion-at-point-functions 'minibuffer
                          'completion-at-point-functions
                   'comint-dynamic-complete-functions)
                 #'node-completion 0 'local))))
  (switch-to-buffer-other-window (*node*)))




(provide 'node)


;; end of node.el
