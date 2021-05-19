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



(provide 'node)


;; end of node.el
