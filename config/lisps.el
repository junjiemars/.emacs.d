;;;; -*- lexical-binding:t -*-
;;;;
;; lisp
;;;;



(defun set-lisp-basic-mode! ()
  ;; structured editing of s-expression data
  (enable-paredit-mode)
  ;; enable automatically adjust the identation of code
  (aggressive-indent-mode)
  ;; hilighting parentheses,brackets,and braces in minor mode
  (rainbow-delimiters-mode))


(provide 'lisps)
