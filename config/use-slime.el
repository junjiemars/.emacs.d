;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime
;;;;


(defmacro set-inferior-lisp-program (lisp &optional force)
  "Safe set inferior-lisp-program var, it must be set before slime start."
  `(if ,force
       (setq% inferior-lisp-program ,lisp slime)
     (when (or (not (string= ,lisp inferior-lisp-program))
							 (string= "lisp" inferior-lisp-program))
       (setq% inferior-lisp-program ,lisp slime))))


(defmacro common-lisp-implementations ()
	"Return a list of existing common-lisp implementations."
	`(remove
		nil
		(list
		 (let ((sbcl (executable-find "sbcl")))
			 (when sbcl
				 (set-inferior-lisp-program "sbcl" t)
				 (list 'sbcl (list sbcl))))
		 (let ((abcl (executable-find "abcl")))
			 (when abcl (list 'abcl (list abcl))))
		 (let ((ecl (executable-find "ecl")))
			 (when ecl (list 'ecl (list ecl)))))))


;;;###autoload
(defun set-slime-lisp-implementations! ()
	"Set `slime-lisp-implementations' with `common-lisp-implementations'."
	(setq% slime-lisp-implementations
				 (common-lisp-implementations) slime))


(provide 'use-slime)

