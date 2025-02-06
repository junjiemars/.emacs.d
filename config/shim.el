;; -*- lexical-binding:t -*-
;;;;
;; Nore Emacs
;; https://github.com/junjiemars/.emacs.d
;;;;
;; shim.el
;;;;
;; Commentary: if-*.
;;;;


(defmacro emacs-home% (&optional file)
  "Return path of FILE under \\='~/.emacs.d\\=' at compile-time."
  (emacs-home* file))

(defmacro v-home% (&optional file)
  "Return versioned path of FILE under \\=`v-home\\=' at compile-time."
  (v-home* file))

(defmacro v-home%> (file)
  "Return the \\=`v-home\\=' FILE with the extension of compiled file."
  (concat (v-home* file) (comp-file-extension%)))

(defmacro v-home! (file)
  "Make versioned path of FILE under \\=`v-home\\=' at compile-time."
  (make-v-home* file))

(defmacro compile-unit% (file &optional only-compile)
  "Make an compile unit at compile time for \\=`compile!\\='"
  (funcall `(lambda () (compile-unit* ,file ,only-compile))))



;;;
;; `gensym*' since Emacs-26+
;;;

(defvar *gensym-counter* 0 "The counter of \\=`gensym*\\='.")

(defun gensym* (&optional prefix)
  "Generate a new uninterned symbol, PREFIX default is \"n\"."
  (make-symbol
   (concat (or prefix "n")
           (prog1 (number-to-string *gensym-counter*)
             (setq *gensym-counter* (1+ *gensym-counter*))))))

;; end of `gensym*'

;;;
;; if-*
;;;

(defvar *nore-obarray* (make-vector 251 nil)
  "Interned obarray at compile-time.")

(defun feature? (feature)
  "Return t if has the FEAUTURE, otherwise nil."
  (let ((has (concat (symbol-name feature) "_ft+"))
        (non (concat (symbol-name feature) "_ft-")))
    (cond ((intern-soft has *nore-obarray*) t)
          ((intern-soft non *nore-obarray*) nil)
          ((require feature nil t) (intern has *nore-obarray*) t)
          (t (intern non *nore-obarray*) nil))))

(defun fn? (fn feature)
  "Return t if the FN of FEATURE is bounded, otherwise nil."
  (let ((has (concat (symbol-name fn) "_fn+"))
        (non (concat (symbol-name fn) "_fn-")))
    (cond ((intern-soft has *nore-obarray*) t)
          ((intern-soft non *nore-obarray*) nil)
          ((fboundp fn) (intern has *nore-obarray*) t)
          (feature (cond ((and (require feature nil t) (fboundp fn))
                          (intern has *nore-obarray*) t)
                         (t (intern non *nore-obarray*) nil)))
          (t (intern non *nore-obarray*) nil))))

(defun var? (var feature)
  "Return t if the VAR of FEATURE is bounded, otherwise nil."
  (declare (indent 3))
  (let ((has (concat (symbol-name var) "_var+"))
        (non (concat (symbol-name var) "_var-")))
    (cond ((intern-soft has *nore-obarray*) t)
          ((intern-soft non *nore-obarray*) nil)
          ((boundp var) (intern has *nore-obarray*) t)
          (feature (cond ((and (require feature nil t) (boundp var))
                          (intern has *nore-obarray*) t)
                         (t (intern non *nore-obarray*) nil)))
          (t (intern non *nore-obarray*) nil))))

;; (defun unintern-if* (name)
;;   "Delete NAME from \\=`*if-obarray*\\='."
;;   (unintern (format "%s_ft+" name) *if-obarray*)
;;   (unintern (format "%s_ft-" name) *if-obarray*)
;;   (unintern (format "%s_fn+" name) *if-obarray*)
;;   (unintern (format "%s_fn-" name) *if-obarray*)
;;   (unintern (format "%s_var+" name) *if-obarray*)
;;   (unintern (format "%s_var-" name) *if-obarray*))

;; end of if-*

;;;
;; key*
;;;

(defun key? (keymap key def)
  "Return KEY if DEF for KEY in KEYMAP, otherwise nil"
  (if (eq def (lookup-key keymap key))
      (cons key def)
    (cons key nil)))

;; end of key*

(provide 'shim)

;; end of shim.el
