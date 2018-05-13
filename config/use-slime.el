;;;; -*- lexical-binding:t -*-
;;;;
;; use-slime
;;;;


(defmacro common-lisp-implementations (&rest lispspec)
	"LISPSPEC:
'sbcl
'\(ecl :init slime-init-comman\)
'\(acl \(\"acl7\" \"-quiet\"\)
			:coding-system emacs-mule\)"
	`(let ((lisps nil))
		 (dolist (x (list ,@lispspec) lisps)
			 (let ((lisp (if (consp x) (car x) x)))
				 (when (executable-find (symbol-name lisp))
					 (if (consp x)
							 (if (consp (cadr x))
									 (push x lisps)
								 (push (list lisp
														 (append
															(list (executable-find (symbol-name lisp)))
															(cdr x)))
											 lisps))
						 (push (list lisp
												 (list (executable-find (symbol-name lisp))))
									 lisps)))))))

(defun set-slime-lisp-implementations! ()
	"More easy way to set `slime-lisp-implementations'."
	(setq% slime-lisp-implementations
				 (common-lisp-implementations 'acl 'ccl 'clasp 'ecl 'sbcl)
				 slime))


(provide 'use-slime)

