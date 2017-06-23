;;;; -*- lexical-binding:t -*-
;;;;
;; strap
;;;;




(defun v-path! (file dir &optional extension)
  "Make the versioned DIR base on the existing FILE's directory 
and return it."
  (v-path* file dir extension))


(defun compile-and-load-elisp-files! (vdir &rest files)
  "Compile and load the elisp FILES, save compiled files in VDIR."
  (declare (indent 1))
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


(defun self-def-files! ()
  (let ((d (emacs-home* "private/"))
        (p (emacs-home* "private/self-path.el"))
        (fs `(,(cons (emacs-home* "private/self-env-spec.el")
                     (emacs-home* "config/sample-self-env-spec.el"))
              ,(cons (emacs-home* "private/self-package-spec.el")
                     (emacs-home* "config/sample-self-package-spec.el"))
              ,(cons (emacs-home* "private/self-prelogure.el")
                     (emacs-home* "config/sample-self-prelogue.el"))
              ,(cons (emacs-home* "private/self-epilogue.el")
                     (emacs-home* "config/sample-self-epilogue.el"))))
        (paths `(:env-spec ,(emacs-home* "private/self-env-spec.el")
                           :package-spec nil
                           :prelogue nil
                           :epilogue nil)))
    (when (not (file-exists-p p))
      (when (not (file-exists-p d)) (make-directory d t))
      (save-sexpr-to-file (list 'def-self-paths (cons 'list paths)) p)
      (dolist (f fs)
        (let ((dst (car f))
              (src (cdr f)))
          (when (not (file-exists-p dst))
            (copy-file src dst t)))))
    paths))


(defvar self-def-paths (self-def-files!))
