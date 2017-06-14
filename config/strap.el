;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;




(defun foo (n) (* n n))


(defun compile-and-load-elisp-files (vdir files)
  "Compile and load the elisp FILES, save compiled files in VDIR."
  (dolist (f files)
    (compile-and-load-elisp-file* vdir f)))
