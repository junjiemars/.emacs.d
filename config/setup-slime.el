;;;;
;; sbcl
;;;;

;; sbcl path
(platform-supported-when
 windows-nt
 (let* ((home (getenv "SBCL_HOME"))
        (path (windows-nt-path
               (file-name-directory
                (if home home (bin-path "sbcl"))))))
   (add-to-list 'exec-path path)
   (safe-setq-inferior-lisp-program "sbcl" t)))



(defun common-lisp-implementations ()
  "Returns a list of common-lisp implementations."
  (let ((sbcl (when (bin-exists-p "sbcl") (bin-path "sbcl")))
        (abcl (when (bin-exists-p "abcl") (bin-path "abcl")))
        (ecl (when (bin-exists-p "ecl") (bin-path "ecl"))))
    (remove nil (list (when sbcl (list 'sbcl (list sbcl)))
                      (when abcl (list 'abcl (list abcl)))
                      (when ecl (list 'ecl (list ecl)))))))



;; setup sbcl, it's slow process so be adviced
(defadvice slime (before slime-before compile)
  (set-default 'slime-lisp-implementations (common-lisp-implementations))
  (add-hook
   'slime-repl-mode-hook
   (lambda ()
     (safe-fn-when slime-close-all-parens-in-sexp
       (local-set-key (kbd "C-c C-]")
                      'slime-close-all-parens-in-sexp))
     (safe-fn-when slime-selector 
       (global-set-key (kbd "C-c s")
                       'slime-selector))))
  (slime-setup '(slime-fancy slime-asdf)))









