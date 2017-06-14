;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;




(defun v-path! (file dir &optional extension)
  "Make the versionized DIR base on the existing FILE's directory 
and return it."
  (v-path* file dir extension))


(defun compile-and-load-elisp-files (vdir files)
  "Compile and load the elisp FILES, save compiled files in VDIR."
  (dolist (f files)
    (compile-and-load-elisp-file* vdir f)))



;; Versionized dirs
(setq-default recentf-save-file (v-home! ".recentf/" "recentf"))
(setq-default savehist-file (v-home! ".minibuffer/" "history"))
