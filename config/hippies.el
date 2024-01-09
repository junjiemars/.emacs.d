;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; hippies.el
;;;;


(defun on-hippie-init! ()
  "On \\=`hippie-exp\\=' initialization."
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



(provide 'hippies)

;; end of hippies.el
