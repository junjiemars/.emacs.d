;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime
;;;;



(defmacro common-lisp-path (name)
  `(platform-supported-if windows-nt
       (windows-nt-posix-path (bin-path ,name))
     (bin-path ,name)))


(defmacro safe-setq-inferior-lisp-program (lisp &optional force)
  "Safe set inferior-lisp-program var, it must be set before slime start."
  `(if (boundp 'inferior-lisp-program)
       (if ,force
           (setq inferior-lisp-program ,lisp)
         (when (or (not (string= ,lisp inferior-lisp-program))
                   (string= "lisp" inferior-lisp-program))
           (setq inferior-lisp-program ,lisp)))
     (setq-default inferior-lisp-program ,lisp)))


(defsubst common-lisp-implementations ()
  "Returns a list of common-lisp implementations. 

use `M-- M-x slime', \\{slime-repl-mode-map}"
  (remove nil
          (list (when (bin-exists-p "sbcl")
                  (safe-setq-inferior-lisp-program "sbcl" t)
                  (list 'sbcl (list (common-lisp-path "sbcl"))))
                (when (bin-exists-p "abcl")
                  (list 'abcl (list (common-lisp-path "abcl"))))
                (when (bin-exists-p "ecl")
                  (list 'ecl (list (common-lisp-path "ecl")))))))



(provide 'use-slime)

