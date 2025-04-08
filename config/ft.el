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

(defmacro if-feature-treesit% (then &rest body)
  (declare (indent 1) (debug t))
  `(if-fn% treesit-available-p nil
           (if (treesit-available-p)
               ,then
             (progn% ,@body))
     (progn% ,@body)))

(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (if-fn% treesit-available-p nil
          (if (treesit-available-p)
              `(progn% ,@body))
    `(comment ,@body)))

;;; `vc'
(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (declare (indent 0))
  (when-fn% vc-dir vc-dir
    `(progn% ,@body)))



(provide 'ft)

;; end of ft.el
