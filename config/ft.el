;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; ft.el
;;;;
;; Commentary: builtin features checking.
;;;;


;;; `eglot' builtin since Emacs-29+
(when-feature% eglot)

;;; `eww' builtin since Emacs-24.4+
(when-feature% eww)

;;; `project' builtin since Emacs-26+
(when-feature% project)

;;; `transient'
(when-feature% transient)

;;; `treesit' builtin since Emacs-29+
;; (when-feature% treesit)
(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (let ((hasfn (intern-function-name 'treesit t))
        (nonfn (intern-function-name 'treesit nil)))
    (cond ((intern-soft hasfn) `(progn% ,@body))
          ((intern-soft nonfn) `(comment ,@body))
          ((when-fn% treesit-available-p nil t)
           (cond ((treesit-available-p) (intern hasfn) `(progn%,@body))
                 (t (intern nonfn) `(comment ,@body))))
          (t (intern nonfn) `(comment ,@body)))))

;;; `vc'
;; (when-feature% vc-dir)
(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (declare (indent 0))
  (let ((hasfn (intern-function-name 'vc-dir t))
        (nonfn (intern-function-name 'vc-dir nil)))
    (cond ((intern-soft hasfn) `(progn% ,@body))
          ((intern-soft nonfn) `(comment ,@body))
          ((when-fn% vc-dir vc-dir t) (intern hasfn) `(progn% ,@body))
          (t (intern nonfn) `(comment ,@body)))))



(provide 'ft)

;; end of ft.el
