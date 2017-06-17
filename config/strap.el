;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;




(defun v-path! (file dir &optional extension)
  "Make the versioned DIR base on the existing FILE's directory 
and return it."
  (v-path* file dir extension))


(defun compile-and-load-elisp-files! (vdir files)
  "Compile and load the elisp FILES, save compiled files in VDIR."
  (dolist (f files)
    (compile-and-load-elisp-file* vdir f)))



;; Versioned dirs
(setq-default recentf-save-file (v-home! ".recentf/" "recentf"))
(setq-default savehist-file (v-home! ".minibuffer/" "history"))


(defmacro save-sexpr-to-file (sexpr file)
  "Save SEXPR to the FILE."
  `(save-excursion
     (let ((sexpr-buffer (find-file-noselect ,file)))
       (set-buffer sexpr-buffer)
       (erase-buffer)
       (print ,sexpr sexpr-buffer)
       (save-buffer)
       (kill-buffer sexpr-buffer))))


(defmacro theme-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     (version-supported-when
         < 23
       ,@body)))


(defmacro font-supported-p (&rest body)
  (declare (indent 1))
  `(graphic-supported-p
     ,@body))


(defvar self-def-files
  (let ((p (emacs-home* "private/self-path.el"))
        (f (emacs-home* "private/self.el")))
    (when (not (file-exists-p p))
      (save-sexpr-to-file (list 'def-self-path f) p))
    (when (not (file-exists-p f))
      (copy-file (emacs-home* "private/self-sample.el") f t))
    (list :path p :file f)))
