;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; feature.el
;;;;
;; Commentary: builtin features checking.
;;;;


;;; `eglot' builtin since Emacs-29+
(defmacro-if-feature% eglot)
(defmacro when-feature-eglot% (&rest body)
  (declare (indent 0))
  (if-feature-eglot%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `eww' builtin since Emacs-24.4+
(defmacro-if-feature% eww)
(defmacro when-feature-eww% (&rest body)
  (declare (indent 0))
  (if-feature-eww%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `project' builtin since Emacs-26+
(defmacro-if-feature% project)
(defmacro when-feature-project% (&rest body)
  (declare (indent 0))
  (if-feature-project%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `transient'
(defmacro-if-feature% transient)
(defmacro when-feature-transient% (&rest body)
  "When \\=`transient\\=', do BODY."
  (declare (indent 0))
  (if-feature-transient%
      `(progn% ,@body)
    `(comment ,@body)))

;;; `treesit' builtin since Emacs-29+
(defmacro-if-feature% treesit)
(defmacro when-feature-treesit% (&rest body)
  (declare (indent 0))
  (if-feature-treesit%
      (if-fn% 'treesit-available-p nil
              (if% (treesit-available-p)
		              `(progn% ,@body)
                `(comment ,@body))
        `(comment ,@body))
    `(comment ,@body)))

;;; `vc'
(defmacro-if-feature% vc)
(defmacro when-feature-vc% (&rest body)
  "When \\=`vc\\=', do BODY."
  (declare (indent 0))
  (if-feature-vc%
      (if-fn% 'vc-dir 'vc-dir
              `(progn% ,@body)
        `(comment ,@body))
    `(comment ,@body)))



(provide 'feature)

;; end of feature.el
