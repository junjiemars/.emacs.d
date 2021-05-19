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
      (user-error* "!%s no found" nvmsh))))



(provide 'node)


;; end of node.el
