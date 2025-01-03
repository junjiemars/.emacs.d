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
(defmacro when-feature-treesit% (&rest body)
  (if-fn% treesit-available-p nil
          (if% (treesit-available-p)
              `(progn% ,@body)
            `(comment ,@body))
    `(comment ,@body)))

;;; `vc'
(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (declare (indent 0))
  (if-fn% vc-dir vc-dir
          `(progn% ,@body)
    `(comment ,@body)))



(provide 'ft)

;; end of ft.el
