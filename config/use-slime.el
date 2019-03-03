;;;; -*- lexical-binding:t -*-
;;;;
;; More reasonable Emacs on MacOS, Windows and Linux
;; https://github.com/junjiemars/.emacs.d
;;;;
;; use-slime.el
;;;;


(defmacro common-lisp-implementations (&rest lispspec)
  "LISPSPEC such as:
'sbcl
'(ecl :init slime-init-command)
'(acl (\"acl7\" \"-quiet\")
      :coding-system emacs-mule)"
  (declare (indent 0))
  `(let ((lisps nil))
     (dolist* (x (list ,@lispspec) lisps)
       (let* ((lisp (if (consp x) (car x) x))
              (bin (executable-find (symbol-name lisp))))
         (when bin
           (if (consp x)
               (if (consp (cadr x))
                   (push x lisps)
                 (push (list lisp (append (list bin) (cdr x))) lisps))
             (push (list lisp (list bin)) lisps)))))))


(defun set-slime-lisp-implementations! ()
  "More easy way to set `slime-lisp-implementations'."
  (setq% slime-lisp-implementations
         (common-lisp-implementations
           'acl
           'ccl
           'clasp
           'ecl
           'sbcl)
         'slime))


(provide 'use-slime)

;; end of file
