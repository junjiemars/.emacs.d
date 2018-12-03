;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; on-hippie-autoload.el
;;;;


(with-eval-after-load 'hippie-exp
  (setq% hippie-expand-try-functions-list
         '(try-complete-file-name-partially
           try-complete-file-name
           try-expand-all-abbrevs
           ;; try-expand-list
           ;; try-expand-line
           try-expand-dabbrev
           try-expand-dabbrev-all-buffers
           try-expand-dabbrev-from-kill
           try-complete-lisp-symbol-partially
           try-complete-lisp-symbol)
         'hippie-exp))


;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(define-key (current-global-map) (kbd "M-/") #'hippie-expand)




