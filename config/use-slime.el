;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime
;;;;



(defmacro common-lisp-path (name)
  `(platform-supported-if windows-nt
       (windows-nt-posix-path (bin-path ,name))
     (bin-path ,name)))


(defmacro set-inferior-lisp-program (lisp &optional force)
  "Safe set inferior-lisp-program var, it must be set before slime start."
  `(if ,force
       (setq% inferior-lisp-program ,lisp slime)
     (when (or (not (string= ,lisp inferior-lisp-program))
	       (string= "lisp" inferior-lisp-program))
       (setq% inferior-lisp-program ,lisp slime))))


(defmacro common-lisp-implementations ()
	"Return a list of common-lisp implementations."
	`(remove nil
					 (list (when (bin-exists-p "sbcl")
									 (set-inferior-lisp-program "sbcl" t)
									 (list 'sbcl (list (common-lisp-path "sbcl"))))
								 (when (bin-exists-p "abcl")
									 (list 'abcl (list (common-lisp-path "abcl"))))
								 (when (bin-exists-p "ecl")
									 (list 'ecl (list (common-lisp-path "ecl")))))))


(defun set-slime-lisp-implementations! ()
	"Set `slime-lisp-implementations' with `common-lisp-implementations'."
	(setq% slime-lisp-implementations
				 (common-lisp-implementations) slime))


(provide 'use-slime)

