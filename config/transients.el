;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; transients.el
;;;;

;;; macro

(defmacro-if-feature% transient)

(defmacro when-feature-transient% (&rest body)
  "When \\=`transient\\=', do BODY."
  (declare (indent 0))
  (if-feature-transient%
      `(progn% ,@body)
  	`(comment ,@body)))

;; end of macro

(when-feature-transient%
  (defun on-transient-init! ()
    (v-home! ".transient/history.el")
    (setq transient-history-file (v-home% ".transient/history.el")
          transient-levels-file (v-home% ".transient/levels.el")
          transient-values-file (v-home%".transient/values.el")
          transient-save-history t)))

;; end of transients.el
