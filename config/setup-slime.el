;;;;
;; sbcl
;;;;




(defmacro common-lisp-path (name)
  `(platform-supported-if
       windows-nt
       (windows-nt-posix-path (bin-path ,name))
     (bin-path ,name)))


(defsubst common-lisp-implementations ()
  "Returns a list of common-lisp implementations. use `M-- M-x slime'."
  (remove nil
          (list (when (bin-exists-p "sbcl")
                  (safe-setq-inferior-lisp-program "sbcl" t)
                  (list 'sbcl (list (common-lisp-path "sbcl"))))
                (when (bin-exists-p "abcl")
                  (list 'abcl (list (common-lisp-path "abcl"))))
                (when (bin-exists-p "ecl")
                  (list 'ecl (list (common-lisp-path "ecl")))))))


(defun set-slime-repl-mode! ()
  (safe-fn-when slime-selector 
    (global-set-key (kbd "C-c s")
                    'slime-selector)))


;; setup sbcl, it's slow process so be adviced
(defadvice slime (before slime-before compile)
  (set-default 'slime-lisp-implementations (common-lisp-implementations))
  (add-hook 'slime-repl-mode-hook #'set-slime-repl-mode!)
  (slime-setup '(slime-fancy slime-asdf)))

