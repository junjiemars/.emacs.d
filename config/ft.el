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

;;; `project' builtin since Emacs-25+
(when-feature% project)

;;; `transient'
(when-feature% transient)

;;; `treesit' builtin since Emacs-29+
(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (if-fn% treesit-available-p nil
          (if (treesit-available-p)
              `(progn% ,@body)
            `(comment ,@body))
    `(comment ,@body)))

;;; `vc'
(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (declare (indent 0))
  (when-fn% vc-dir vc-dir
    `(progn% ,@body)))

;;; `xref-find-definitions' since emacs-25+
(eval-when-compile
  (defmacro when-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(progn% ,@body)
      `(comment ,@body))))
(eval-when-compile
  (defmacro unless-xref-find-definitions% (&rest body)
    (declare (indent 0))
    (if-fn% xref-find-definitions xref
            `(comment ,@body)
      `(progn% ,@body))))



(provide 'ft)

;; end of ft.el
